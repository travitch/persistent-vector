-- | This is a port of the persistent vector from clojure to Haskell.
-- It is spine-strict and lazy in the elements.
--
-- The implementation is based on array mapped tries.  The complexity
-- bounds given are mostly O(1), but only if you are willing to accept
-- that the tree cannot have height greater than 7 on 32 bit systems
-- and maybe 8 on 64 bit systems.
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -ddump-simpl #-}
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
  unsafeIndexA,
  unsafeIndex#,
  -- * Modification
  update,
  (//),
  -- * Folds
  foldr,
  foldl,
  -- * Transformations
  map,
  reverse,
  -- * Searches
  filter,
  partition
  ) where

import Prelude hiding
  ( null, length, tail, take
  , drop, map, foldr, foldl
  , reverse, splitAt, filter )

import qualified Control.Applicative as Ap
import Control.DeepSeq
import Data.Bits
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Semigroup as Sem
import qualified Data.Traversable as T

import Data.Vector.Persistent.Array ( Array )
import qualified Data.Vector.Persistent.Array as A

-- Note: using Int here doesn't give the full range of 32 bits on a 32
-- bit machine (it is fine on 64)
data Vector a
  = RootNode { vecSize :: !Int
             , vecShift :: !Int
             , vecTail :: ![a]
             , intVecPtrs :: !(Array (Vector_ a))
             }
  deriving Show

data Vector_ a
    = InternalNode { intVecPtrs_ :: !(Array (Vector_ a))
                   }
    | DataNode { dataVec :: !(Array a)
               }
  deriving Show

instance Eq a => Eq (Vector a) where
  (==) = pvEq

instance Eq a => Eq (Vector_ a) where
  (==) = pvEq_

instance Ord a => Ord (Vector a) where
  compare = pvCompare

instance Ord a => Ord (Vector_ a) where
  compare = pvCompare_

instance F.Foldable Vector where
  foldMap = T.foldMapDefault
  foldr = foldr
  foldl = foldl

instance Functor Vector where
  fmap = map

instance Sem.Semigroup (Vector a) where
  (<>) = append

instance Monoid (Vector a) where
  mempty = empty
  -- Defined for compatibility with ghc 8.2
  mappend = (<>)

instance T.Traversable Vector where
  traverse = pvTraverse

instance NFData a => NFData (Vector a) where
  rnf = pvRnf

instance NFData a => NFData (Vector_ a) where
  rnf = pvRnf_

{-# INLINABLE pvEq #-}
pvEq :: Eq a => Vector a -> Vector a -> Bool
pvEq (RootNode sz1 sh1 t1 v1) (RootNode sz2 sh2 t2 v2) =
  sz1 == sz2 && (sz1 == 0 || (sh1 == sh2 && t1 == t2 && v1 == v2))

{-# INLINABLE pvEq_ #-}
pvEq_ :: Eq a => Vector_ a -> Vector_ a -> Bool
pvEq_ (DataNode a1) (DataNode a2) = a1 == a2
pvEq_ (InternalNode a1) (InternalNode a2) = a1 == a2
pvEq_ _ _ = False

{-# INLINABLE pvCompare #-}
pvCompare :: Ord a => Vector a -> Vector a -> Ordering
pvCompare (RootNode sz1 _ t1 v1) (RootNode sz2 _ t2 v2) =
  compare sz1 sz2 <> if sz1 == 0 then EQ else compare v1 v2 <> compare t1 t2

{-# INLINABLE pvCompare_ #-}
pvCompare_ :: Ord a => Vector_ a -> Vector_ a -> Ordering
pvCompare_ (DataNode a1) (DataNode a2) = compare a1 a2
pvCompare_ (InternalNode a1) (InternalNode a2) = compare a1 a2
pvCompare_ (DataNode _) (InternalNode _) = LT
pvCompare_ (InternalNode _) (DataNode _) = GT


{-# INLINABLE map #-}
-- | \( O(n) \) Map over the vector
map :: (a -> b) -> Vector a -> Vector b
map f = go
  where
    go (RootNode sz sh t v) =
      let t' = L.map f t
          v' = A.map go_ v
      in RootNode sz sh t' v'

    go_ (DataNode v) = DataNode (A.map f v)
    go_ (InternalNode v) = InternalNode (A.map go_ v)

{-# INLINABLE foldr #-}
foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr f = go
  where
    go seed RootNode{vecSize = 0} = seed
    go seed (RootNode _ _ t as) = {-# SCC "gorRootNode" #-}
      let tseed = F.foldl (flip f) seed t
      in A.foldr go_ tseed as
    go_ (DataNode a) seed = {-# SCC "gorDataNode" #-} A.foldr f seed a
    go_ (InternalNode as) seed = {-# SCC "gorInternalNode" #-}
      A.foldr go_ seed as

{-# INLINABLE foldl #-}
foldl :: (b -> a -> b) -> b -> Vector a -> b
foldl f = go
  where
    go seed RootNode{vecSize = 0} = seed
    go seed (RootNode _ _ t as) =
      let rseed = A.foldl go_ seed as
      in F.foldr (flip f) rseed t

    go_ seed (DataNode a) = {-# SCC "golDataNode" #-} A.foldl f seed a
    go_ seed (InternalNode as) =
      A.foldl go_ seed as

{-# INLINABLE pvTraverse #-}
pvTraverse :: Ap.Applicative f => (a -> f b) -> Vector a -> f (Vector b)
pvTraverse f = go
  where
    go RootNode{vecSize = 0} = Ap.pure empty
    go (RootNode sz sh t as) =
      Ap.liftA2 (RootNode sz sh) (T.traverse f t) (A.traverseArray go_ as)
    go_ (DataNode a) = DataNode Ap.<$> A.traverseArray f a
    go_ (InternalNode as) = InternalNode Ap.<$> A.traverseArray go_ as

{-# INLINABLE append #-}
append :: Vector a -> Vector a -> Vector a
append v1 v2
  | null v1 = v2
  | null v2 = v1
append v1 v2 = F.foldl' snoc v1 v2

{-# INLINABLE pvRnf #-}
pvRnf :: NFData a => Vector a -> ()
pvRnf RootNode{vecSize = 0} = ()
pvRnf (RootNode _ _ t as) = rnf as `seq` rnf t

{-# INLINABLE pvRnf_ #-}
pvRnf_ :: NFData a => Vector_ a -> ()
pvRnf_ (DataNode a) = rnf a
pvRnf_ (InternalNode a) = rnf a

-- Functions

-- | The empty vector. The shift chosen here is not principled; there
-- may be one we can use that lets us treat empty and nonempty vectors
-- uniformly, but I haven't figured out how to do that yet.
empty :: Vector a
empty = RootNode 0 0 [] A.empty

-- | Test to see if the vector is empty. (O(1))
null :: Vector a -> Bool
null xs = length xs == 0

-- | Get the length of the vector. (O(1))
length :: Vector a -> Int
length RootNode { vecSize = s } = s

-- | Bounds-checked indexing into a vector. (O(1))
index :: Vector a -> Int -> Maybe a
index v ix
  | length v > ix = unsafeIndexA v ix
  | otherwise = Nothing

-- Index into a list from the rear.
--
-- revIx# [1..3] 0 = (# 3 #)
-- revIx# [1..3] 1 = (# 2 #)
-- revIx# [1..3] 2 = (# 1 #)
--
-- This is the same as reversing the list and then indexing
-- into it, but it doesn't need to allocate a reversed copy
-- of the list.
--
-- TODO: produce an error if the index is too large, instead of
-- just giving a wrong answer. This just requires a custom
-- version of `drop`.
revIx# :: [a] -> Int -> (# a #)
revIx# xs i = go xs (L.drop (i + 1) xs)
  where
    go :: [a] -> [b] -> (# a #)
    go (a : _) [] = (# a #)
    go (_ : as) (_ : bs) = go as bs
    go _ _ = error "revIx#: Whoopsy!"

unsafeIndex :: Vector a -> Int -> a
unsafeIndex vec ix
  | (# a #) <- unsafeIndex# vec ix
  = a

unsafeIndexA :: Applicative f => Vector a -> Int -> f a
{-# INLINABLE unsafeIndexA #-}
unsafeIndexA vec ix
  | (# a #) <- unsafeIndex# vec ix
  = pure a

-- | Unchecked indexing into a vector. (O(1))
--
-- Note that out-of-bounds indexing might not even crash - it will
-- usually just return nonsense values.
unsafeIndex# :: Vector a -> Int -> (# a #)
unsafeIndex# vec ix
  | ix >= tailOffset vec =
    (vecTail vec) `revIx#` (ix .&. 0x1f)
  | otherwise =
      let sh = vecShift vec
      in go (sh - 5) (A.index (intVecPtrs vec) (ix `shiftR` sh))
  where
    go level v
      | level == 0 = A.index# (dataVec v) (ix .&. 0x1f)
      | otherwise =
        let nextVecIx = (ix `shiftR` level) .&. 0x1f
            v' = intVecPtrs_ v
        in go (level - 5) (A.index v' nextVecIx)

-- | Construct a vector with a single element. (O(1))
singleton :: a -> Vector a
singleton elt =
  RootNode { vecSize = 1
           , vecShift = 5
           , vecTail = [elt]
           , intVecPtrs = A.empty
           }

arraySnoc :: Array a -> a -> Array a
arraySnoc a elt = A.run $ do
  let alen = A.length a
  a' <- A.new_ (1 + alen)
  A.copy a 0 a' 0 alen
  A.write a' alen elt
  return a'

-- | Append an element to the end of the vector. (O(1))
snoc :: Vector a -> a -> Vector a
snoc v elt
  | null v = singleton elt
snoc v@RootNode { vecSize = sz, vecShift = sh, vecTail = t } elt
  -- Room in tail
  | sz .&. 0x1f /= 0 = v { vecTail = elt : t, vecSize = sz + 1 }
  -- Overflow current root
  | sz `shiftR` 5 > 1 `shiftL` sh =
    RootNode { vecSize = sz + 1
             , vecShift = sh + 5
             , vecTail = [elt]
             , intVecPtrs = A.fromList 2 [ InternalNode (intVecPtrs v)
                                         , newPath sh t
                                         ]
             }
  -- Insert into the tree
  | otherwise =
      RootNode { vecSize = sz + 1
               , vecShift = sh
               , vecTail = [elt]
               , intVecPtrs = pushTail sz t sh (intVecPtrs v)
               }

-- | A recursive helper for 'snoc'.  This finds the place to add new
-- elements.
pushTail :: Int -> [a] -> Int -> Array (Vector_ a) -> Array (Vector_ a)
pushTail cnt t = go
  where
    go level parent
      | level == 5 = arraySnoc parent (DataNode (A.fromListRev 32 t))
      | subIdx < A.length parent =
        let nextVec = A.index parent subIdx
            toInsert = go (level - 5) (intVecPtrs_ nextVec)
        in A.update parent subIdx (InternalNode toInsert)
      | otherwise = arraySnoc parent (newPath (level - 5) t)
      where
        subIdx = ((cnt - 1) `shiftR` level) .&. 0x1f

-- | The other recursive helper for 'snoc'.  This one builds out a
-- sub-tree to the current depth.
newPath :: Int -> [a] -> Vector_ a
newPath level t
  | level == 0 = DataNode (A.fromListRev 32 t)
  | otherwise = InternalNode $ A.fromList 1 $ [newPath (level - 5) t]

-- | \( O(1) \) Update a single element at @ix@ with new value @elt@ in @v@. (O(1))
--
-- > update ix elt v
update :: Int -> a -> Vector a -> Vector a
update ix elt = (// [(ix, elt)])

-- | \( O(n) \) Bulk update.
--
-- > v // updates
--
-- For each @(index, element)@ pair in @updates@, modify @v@ such that
-- the @index@th position of @v@ is @element@.
-- Indices in @updates@ that are not in @v@ are ignored
(//) :: Vector a -> [(Int, a)] -> Vector a
(//)  = L.foldr replaceElement

replaceElement :: (Int, a) -> Vector a -> Vector a
replaceElement (ix, elt) v@(RootNode { vecSize = sz, vecShift = sh, vecTail = t })
  -- Invalid index
  | sz <= ix || ix < 0 = v
  -- Item is in tail,
  | ix >= toff =
    let tix = sz - 1 - ix
        (keepHead, _:keepTail) = L.splitAt tix t
    in v { vecTail = keepHead ++ (elt : keepTail) }
  -- Otherwise the item to be replaced is in the tree
  | otherwise = v { intVecPtrs = go sh (intVecPtrs v) }
  where
    toff = tailOffset v
    go level vec
      -- At the data level, modify the vector and start propagating it up
      | level == 5 =
        let dnode = DataNode $ A.update (dataVec vec') (ix .&. 0x1f) elt
        in A.update vec vix dnode
      -- In the tree, find the appropriate sub-array, call
      -- recursively, and re-allocate current array
      | otherwise =
          let rnode = go (level - 5) (intVecPtrs_ vec')
          in A.update vec vix (InternalNode rnode)
      where
        vix = (ix `shiftR` level) .&. 0x1f
        vec' = A.index vec vix

tailOffset :: Vector a -> Int
tailOffset v
  | len < 32 = 0
  | otherwise = (len - 1) `shiftR` 5 `shiftL` 5
  where
    len = length v

-- | O(n) Reverse a vector
reverse :: Vector a -> Vector a
reverse = fromList . F.foldl' (flip (:)) []

-- | O(n) Filter according to the predicate
filter :: (a -> Bool) -> Vector a -> Vector a
filter p = F.foldl' go empty
  where
    go acc e = if p e then snoc acc e else acc

-- | \( O(n) \) Return the elements that do and do not obey the predicate
partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
partition p v0 = case F.foldl' go (TwoVec empty empty) v0 of
  TwoVec v1 v2 -> (v1, v2)
  where
    go (TwoVec atrue afalse) e =
      if p e then TwoVec (snoc atrue e) afalse else TwoVec atrue (snoc afalse e)

data TwoVec a = TwoVec !(Vector a) !(Vector a)

-- | \( O(n) \) Construct a vector from a list. (O(n))
fromList :: [a] -> Vector a
fromList = F.foldl' snoc empty
