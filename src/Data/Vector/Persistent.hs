-- | This is a port of the persistent vector from clojure to Haskell.
-- It is spine-strict and lazy in the elements.
--
-- The implementation is based on array mapped tries.  The complexity
-- bounds given are mostly O(1), but only if you are willing to accept
-- that the tree cannot have height greater than 7 on 32 bit systems
-- and maybe 8 on 64 bit systems.
module Data.Vector.Persistent (
  Vector,
  -- * Construction
  empty,
  singleton,
  snoc,
  fromList,
  -- * Queries
  null,
  length,
  -- * Indexing
  index,
  unsafeIndex,
  take,
  drop,
  splitAt,
  -- * Slicing
  slice,
  shrink,
  -- * Modification
  update,
  (//),
  -- * Folds
  foldr,
  foldl',
  -- * Transformations
  map,
  reverse,
  -- * Searches
  filter,
  partition
  ) where

import Prelude hiding ( null, length, tail, take,
                        drop, map, foldr, reverse,
                        splitAt, filter
                      )

import Control.Applicative hiding ( empty )
import Control.DeepSeq
import Data.Bits
import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Monoid ( Monoid )
import qualified Data.Monoid as M
import Data.Traversable ( Traversable )
import qualified Data.Traversable as T

import Data.Vector.Persistent.Array ( Array )
import qualified Data.Vector.Persistent.Array as A

-- Note: using Int here doesn't give the full range of 32 bits on a 32
-- bit machine (it is fine on 64)

-- | Persistent vectors based on array mapped tries
data Vector a = EmptyVector
              | RootNode { vecSize :: {-# UNPACK #-} !Int
                         , vecShift :: {-# UNPACK #-} !Int
                         , vecOffset :: {-# UNPACK #-} !Int
                         , vecCapacity :: {-# UNPACK #-} !Int
                         , vecTail :: ![a]
                         , intVecPtrs :: {-# UNPACK #-} !(Array (Vector a))
                         }
              | InternalNode { intVecPtrs :: {-# UNPACK #-} !(Array (Vector a))
                             }
              | DataNode { dataVec :: {-# UNPACK #-} !(Array a)
                         }
              deriving (Show)

instance (Eq a) => Eq (Vector a) where
  (==) = pvEq

instance (Ord a) => Ord (Vector a) where
  compare = pvCompare

instance Foldable Vector where
  foldr = foldr

instance Functor Vector where
  fmap = map

instance Monoid (Vector a) where
  mempty = empty
  mappend = append

instance Traversable Vector where
  traverse = pvTraverse

instance (NFData a) => NFData (Vector a) where
  rnf = pvRnf

{-# INLINABLE pvEq #-}
-- | A dispatcher between various equality tests.  The length check is
-- extremely cheap.  There is another optimized check for the case
-- where neither input is sliced.  For sliced inputs, we currently
-- fall back to a list conversion.
pvEq :: (Eq a) => Vector a -> Vector a -> Bool
pvEq EmptyVector EmptyVector = True
pvEq v1@RootNode { } v2@RootNode { }
  | length v1 /= length v2 = False
  | isNotSliced v1 && isNotSliced v2 = pvSimpleEq v1 v2
  | otherwise = F.toList v1 == F.toList v2
pvEq (DataNode a1) (DataNode a2) = a1 == a2
pvEq (InternalNode a1) (InternalNode a2) = a1 == a2
pvEq _ _ = False

-- | A simple equality implementation for unsliced vectors.  This can
-- proceed structurally.
pvSimpleEq :: (Eq a) => Vector a -> Vector a -> Bool
pvSimpleEq EmptyVector EmptyVector = True
pvSimpleEq (RootNode sz1 sh1 _ _ t1 v1) (RootNode sz2 sh2 _ _ t2 v2) =
  sz1 == sz2 && sh1 == sh2 && t1 == t2 && v1 == v2
pvSimpleEq (DataNode a1) (DataNode a2) = a1 == a2
pvSimpleEq (InternalNode a1) (InternalNode a2) = a1 == a2
pvSimpleEq _ _ = False

{-# INLINABLE pvCompare #-}
-- | A dispatcher for comparison tests
pvCompare :: (Ord a) => Vector a -> Vector a -> Ordering
pvCompare EmptyVector EmptyVector = EQ
pvCompare (DataNode a1) (DataNode a2) = compare a1 a2
pvCompare (InternalNode a1) (InternalNode a2) = compare a1 a2
pvCompare v1@RootNode { vecSize = s1 } v2@RootNode { vecSize = s2 }
  | s1 /= s2 = compare s1 s2
  | isNotSliced v1 && isNotSliced v2 = pvSimpleCompare v1 v2
  | otherwise = compare (F.toList v1) (F.toList v2)
pvCompare EmptyVector _ = LT
pvCompare _ EmptyVector = GT
pvCompare (DataNode _) (InternalNode _) = LT
pvCompare (InternalNode _) (DataNode _) = GT
pvCompare _ _ = error "Data.Vector.Persistent: unexpected root node"



pvSimpleCompare :: (Ord a) => Vector a -> Vector a -> Ordering
pvSimpleCompare EmptyVector EmptyVector = EQ
pvSimpleCompare (RootNode _ _ _ _ t1 v1) (RootNode _ _ _ _ t2 v2) =
  case compare v1 v2 of
    EQ -> compare t1 t2
    o -> o
pvSimpleCompare (DataNode a1) (DataNode a2) = compare a1 a2
pvSimpleCompare (InternalNode a1) (InternalNode a2) = compare a1 a2
pvSimpleCompare EmptyVector _ = LT
pvSimpleCompare _ EmptyVector = GT
pvSimpleCompare (InternalNode _) (DataNode _) = GT
pvSimpleCompare (DataNode _) (InternalNode _) = LT
pvSimpleCompare _ _ = error "Data.Vector.Persistent.pvSimpleCompare: Unexpected mismatch"


{-# INLINABLE map #-}
-- | O(n) Map over the vector
map :: (a -> b) -> Vector a -> Vector b
map f = go
  where
    go EmptyVector = EmptyVector
    go (DataNode v) = DataNode (A.map f v)
    go (InternalNode v) = InternalNode (A.map (fmap f) v)
    go (RootNode sz sh off cap t v) =
      let t' = L.map f t
          v' = A.map (fmap f) v
      in RootNode sz sh off cap t' v'

{-# INLINABLE foldr #-}
-- | O(n) Right fold over the vector
foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr _ s0 EmptyVector = s0
foldr f s0 v
  | isNotSliced v = sgo v s0
  | otherwise =
    case go v (s0, max 0 (vecCapacity v - vecSize v), length v) of (r, _, _) -> r
  where
    go EmptyVector seed = seed
    go (DataNode a) (seed, nskip, len)
      | len <= 0 = (seed, 0, 0)
      | nskip == 0 = (A.boundedFoldr f (32 - len) 32 seed a, 0, len - A.length a)
      | nskip >= 32 = (seed, nskip - 32, len)
      | otherwise =
        let end = min (max 0 (32 - nskip)) 32
            start = 32 - (len + nskip)
            taken = end - max 0 start
        in (A.boundedFoldr f start end seed a, 0, len - taken)
    go (InternalNode as) seed =
      A.foldr go seed as
      -- Note: if there is a tail at all, the elements are live (slice
      -- drops unused tail elements)
    go (RootNode _ _ _ _ t as) (s, nskip, l) =
      let tseed = L.foldl' (flip f) s t
          seed = (tseed, nskip, l - L.length t)
      in A.foldr go seed as

    -- A simpler variant for unsliced vectors (the common case) that is
    -- significantly more efficient
    sgo EmptyVector seed = seed
    sgo (DataNode a) seed = A.foldr f seed a
    sgo (InternalNode as) seed = A.foldr sgo seed as
    sgo (RootNode _ _ _ _ t as) seed =
      let tseed = L.foldl' (flip f) seed t
      in A.foldr sgo tseed as

{-# INLINABLE foldl' #-}
-- | O(n) Strict left fold over the vector
foldl' :: (b -> a -> b) -> b -> Vector a -> b
foldl' _ s0 EmptyVector = s0
foldl' f s0 v
  | isNotSliced v = sgo s0 v
  | otherwise =
    case go (s0, vecOffset v, length v) v of (r, _, _) -> r
  where
    go seed EmptyVector = seed
    go (seed, nskip, len) (DataNode a)
      | len <= 0 = (seed, 0, 0)
      | nskip == 0 = (A.boundedFoldl' f 0 (min len 32) seed a, 0, len - A.length a)
      | nskip >= 32 = (seed, nskip - 32, len)
      | otherwise =
        let end = min 32 (len + nskip)
            start = nskip
            taken = end - max 0 start
        in (A.boundedFoldl' f start end seed a, 0, len - taken)
    go seed (InternalNode as) =
      A.foldl' go seed as
    go (s, nskip, l) (RootNode _ _ _ _ t as) =
      let (rseed, _, _) = A.foldl' go (s, nskip, l - L.length t) as
      in (L.foldr (flip f) rseed t, 0, 0)

    sgo seed EmptyVector = seed
    sgo seed (DataNode a) = A.foldl' f seed a
    sgo seed (InternalNode as) =
      A.foldl' sgo seed as
    sgo seed (RootNode _ _ _ _ t as) =
      let rseed = A.foldl' sgo seed as
      in F.foldr (flip f) rseed t

{-# INLINABLE pvTraverse #-}
pvTraverse :: (Applicative f) => (a -> f b) -> Vector a -> f (Vector b)
pvTraverse f = go
  where
    go EmptyVector = pure EmptyVector
    go (DataNode a) = DataNode <$> A.traverse f a
    go (InternalNode as) = InternalNode <$> A.traverse go as
    go (RootNode sz sh off cap t as) =
      RootNode sz sh off cap <$> T.traverse f t <*> A.traverse go as

{-# INLINABLE append #-}
append :: Vector a -> Vector a -> Vector a
append EmptyVector v = v
append v EmptyVector = v
append v1 v2 = foldl' snoc v1 v2

{-# INLINABLE pvRnf #-}
pvRnf :: (NFData a) => Vector a -> ()
pvRnf = F.foldr deepseq ()

-- | O(1) The empty vector
empty :: Vector a
empty = EmptyVector

-- | O(1) Test to see if the vector is empty.
null :: Vector a -> Bool
null EmptyVector = True
null _ = False

-- | O(1) Get the length of the vector.
length :: Vector a -> Int
length EmptyVector = 0
length RootNode { vecSize = s, vecOffset = off } = s - off
length InternalNode {} = error "Data.Vector.Persistent.length: Internal nodes should not be exposed"
length DataNode {} = error "Data.Vector.Persistent.length: Data nodes should not be exposed"

-- | O(1) Bounds-checked indexing into a vector.
index :: Vector a -> Int -> Maybe a
index v ix
  | length v > ix = Just $ unsafeIndex v ix
  | otherwise = Nothing

-- | O(1) Unchecked indexing into a vector.
--
-- Note that out-of-bounds indexing might not even crash - it will
-- usually just return nonsense values.
unsafeIndex :: Vector a -> Int -> a
unsafeIndex vec userIndex
--  | tailOffset vec < vecOffset vec = L.reverse (vecTail vec) !! (userIndex .&. 0x1f)
  | ix >= tailOffset vec && vecCapacity vec < vecSize vec =
    L.reverse (vecTail vec) !! (ix .&. 0x1f)
  | otherwise = go (vecShift vec) vec
  where
    -- The user is indexing from zero but there could be some masked
    -- portion of the vector due to the offset - we have to correct to
    -- an internal offset
    ix = vecOffset vec + userIndex
    go level v
      | level == 0 = A.index (dataVec v) (ix .&. 0x1f)
      | otherwise =
        let nextVecIx = (ix `shiftR` level) .&. 0x1f
            v' = intVecPtrs v
        in go (level - 5) (A.index v' nextVecIx)

-- | O(1) Construct a vector with a single element.
singleton :: a -> Vector a
singleton elt =
  RootNode { vecSize = 1
           , vecShift = 5
           , vecOffset = 0
           , vecCapacity = 0
           , vecTail = [elt]
           , intVecPtrs = A.fromList 0 []
           }

-- | A helper to copy an array and add an element to the end.
arraySnoc :: Array a -> a -> Array a
arraySnoc a elt = A.run $ do
  let alen = A.length a
  a' <- A.new_ (1 + alen)
  A.copy a 0 a' 0 alen
  A.write a' alen elt
  return a'

-- | O(1) Append an element to the end of the vector.
snoc :: Vector a -> a -> Vector a
snoc EmptyVector elt = singleton elt
snoc v@RootNode { vecSize = sz, vecShift = sh, vecOffset = off, vecTail = t } elt
  -- In this case, we are operating on a slice that has free space at
  -- the end inside of its tree.  Use 'update' to replace the formerly
  -- unreachable element and then make it reachable.
  | vecCapacity v >= sz =
    let v' = v { vecSize = sz + 1 }
    in update (sz - off) elt v'
  | sz .&. 0x1f /= 0 = v { vecTail = elt : t, vecSize = sz + 1 }
  -- Overflow current root
  | sz `shiftR` 5 > 1 `shiftL` sh =
    RootNode { vecSize = sz + 1
             , vecShift = sh + 5
             , vecOffset = vecOffset v
             , vecCapacity = vecCapacity v + 32
             , vecTail = [elt]
             , intVecPtrs = A.fromList 2 [ InternalNode (intVecPtrs v)
                                         , newPath sh t
                                         ]
             }
  -- Insert into the tree
  | otherwise =
      RootNode { vecSize = sz + 1
               , vecShift = sh
               , vecOffset = vecOffset v
               , vecCapacity = vecCapacity v + 32
               , vecTail = [elt]
               , intVecPtrs = pushTail sz t sh (intVecPtrs v)
               }
snoc _ _ = error "Data.Vector.Persistent.snoc: Internal nodes should not be exposed to the user"

-- | A recursive helper for 'snoc'.  This finds the place to add new
-- elements.
pushTail :: Int -> [a] -> Int -> Array (Vector a) -> Array (Vector a)
pushTail cnt t = go
  where
    go level parent
      | level == 5 = arraySnoc parent (DataNode (A.fromList 32 (L.reverse t)))
      | subIdx < A.length parent =
        let nextVec = A.index parent subIdx
            toInsert = go (level - 5) (intVecPtrs nextVec)
        in A.update parent subIdx (InternalNode toInsert)
      | otherwise = arraySnoc parent (newPath (level - 5) t)
      where
        subIdx = ((cnt - 1) `shiftR` level) .&. 0x1f

-- | The other recursive helper for 'snoc'.  This one builds out a
-- sub-tree to the current depth.
newPath :: Int -> [a] -> Vector a
newPath level t
  | level == 0 = DataNode (A.fromList 32 (L.reverse t))
  | otherwise = InternalNode $ A.fromList 1 $ [newPath (level - 5) t]

-- | O(1) Update a single element at @ix@ with new value @elt@ in
-- @v@.
--
-- > update ix elt v
update :: Int -> a -> Vector a -> Vector a
update ix elt = (// [(ix, elt)])

-- | O(n) Bulk update.
--
-- > v // updates
--
-- For each (index, element) pair in @updates@, modify @v@ such that
-- the @index@th position of @v@ is @element@.
-- Indices in @updates@ that are not in @v@ are ignored
(//) :: Vector a -> [(Int, a)] -> Vector a
(//) = L.foldr replaceElement

replaceElement :: (Int, a) -> Vector a -> Vector a
replaceElement _ EmptyVector = EmptyVector
replaceElement (userIndex, elt) v@(RootNode { vecSize = sz, vecShift = sh, vecTail = t })
  -- Invalid index
  | sz <= ix || ix < 0 = v
  -- Item is in tail,
  | ix >= toff && vecCapacity v < sz =
    case t of
      -- The tail can only be empty if this was a slice where the last
      -- array in the tree is full and the slice left no tail.  This
      -- is rare but we have to handle it.
      [] -> v { vecTail = [elt] }
      _ ->
        let tix = sz - 1 - ix
            (keepHead, _:keepTail) = L.splitAt tix t
        in v { vecTail = keepHead ++ (elt : keepTail) }
  -- Otherwise the item to be replaced is in the tree
  | otherwise = v { intVecPtrs = go sh (intVecPtrs v) }
  where
    ix = userIndex + vecOffset v
    toff = tailOffset v
    go level vec
      -- At the data level, modify the vector and start propagating it up
      | level == 5 =
        let dnode = DataNode $ A.update (dataVec vec') (ix .&. 0x1f) elt
        in A.update vec vix dnode
      -- In the tree, find the appropriate sub-array, call
      -- recursively, and re-allocate current array
      | otherwise =
          let rnode = go (level - 5) (intVecPtrs vec')
          in A.update vec vix (InternalNode rnode)
      where
        vix = (ix `shiftR` level) .&. 0x1f
        vec' = A.index vec vix
replaceElement _ _ = error "Data.Vector.Persistent.replaceElement: should not see internal nodes"

-- | O(1) Return a slice of @v@ of length @length@ starting at index
-- @start@.  The returned vector may have fewer than @length@ elements
-- if the bounds are off on either side (the start is negative or
-- length takes it past the end).
--
-- A slice of negative or zero length is the empty vector.
--
-- > slice start length v
--
-- Note that a slice retains all of the references that the vector it
-- is derived from has.  They are not reachable via any traversals and
-- are not counted towards its size, but this may lead to references
-- living longer than intended.  If is important to you that this not
-- happen, call 'shrink' on the return value of 'slice' to drop unused
-- space and references.
slice :: Int -> Int -> Vector a -> Vector a
slice _ _ EmptyVector = EmptyVector
slice start userLen v@RootNode { vecSize = sz, vecOffset = off, vecCapacity = cap, vecTail = t }
  | len <= 0 = EmptyVector
  -- All the retained data is in the tail, so zero everything else out
  | toff < start =
    let t' = L.reverse $ L.take userLen $ L.drop (start - toff) $ L.reverse t
    in v { vecOffset = 0
         , vecCapacity = 0
         , intVecPtrs = A.fromList 0 []
         , vecSize = L.length t'
         , vecTail = t'
         }
  -- Start was negative, so we really start at zero and retain at most
  -- (len + start) elements.  In this case vecOffset remains the same.
  | start < 0 =
    let eltsRetained = min (len + start) sz
    in v { vecSize = eltsRetained
         , vecTail = L.drop (sz - eltsRetained) t
         }
  -- If capacity < start, the tail needs to be modified from the front
  -- in fact, max 0 (start - capacity) items need to be dropped from the
  -- list
  | otherwise =
      let newOff = off + start
          newSize = min (newOff + len) sz
          ntake = max 0 (start - cap)
          t' = L.drop (sz - newSize) t
      in v { vecOffset = newOff
           , vecSize = newSize
           , vecTail = L.take (L.length t' - ntake) t'
           }
  where
    toff = tailOffset v
    len = max 0 (min userLen (sz - start))
slice _ _ _ = error "Data.Vector.Persistent.slice: Internal node"

-- Note that slice removes unneeded elements from the tail so that
-- snoc can mostly work unchanged.  snoc does need to change if the
-- slice takes so many elements that parts of the tree contain
-- inaccessible elements.  In that case, just use update instead.

-- | O(1) Take the first @i@ elements of the vector.
--
-- Note that this is just a wrapper around slice and the resulting
-- slice retains references that are inaccessible.  Use 'shrink' if
-- this is undesirable.
take :: Int -> Vector a -> Vector a
take = slice 0

-- | O(1) Drop @i@ elements from the front of the vector.
--
-- Note that this is just a wrapper around slice.
drop :: Int -> Vector a -> Vector a
drop i v = slice i (length v) v

-- | O(1) Split the vector at the given position.
splitAt :: Int -> Vector a -> (Vector a, Vector a)
splitAt ix v = (take ix v, drop ix v)

-- | O(n) Force a sliced vector to drop any unneeded space and
-- references.
--
-- This is a no-op for an un-sliced vector.
shrink :: Vector a -> Vector a
shrink EmptyVector = EmptyVector
shrink v
  | isNotSliced v = v
  | otherwise = fromList $ F.toList v

-- | O(n) Reverse a vector
reverse :: Vector a -> Vector a
reverse = fromList . foldl' (flip (:)) []

-- | O(n) Filter according to the predicate
filter :: (a -> Bool) -> Vector a -> Vector a
filter p = foldl' go empty
  where
    go acc e = if p e then snoc acc e else acc

-- | O(n) Return the elements that do and do not obey the predicate
partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
partition p = foldl' go (empty, empty)
  where
    go (atrue, afalse) e =
      if p e then (snoc atrue e, afalse) else (atrue, snoc afalse e)

-- | O(n) Construct a vector from a list.
fromList :: [a] -> Vector a
fromList = F.foldl' snoc empty

-- Helpers

tailOffset :: Vector a -> Int
tailOffset EmptyVector = 0
tailOffset v
  | len < 32 = 0
  | otherwise = (len - 1) `shiftR` 5 `shiftL` 5
  where
    len = vecSize v

isNotSliced :: Vector a -> Bool
isNotSliced v = vecOffset v == 0 && vecCapacity v < vecSize v