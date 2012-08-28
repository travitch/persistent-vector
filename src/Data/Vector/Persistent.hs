-- | This is a port of the persistent vector from clojure to Haskell.
-- It is spine-strict and lazy in the elements.
--
-- The implementation is based on array mapped tries.  The complexity
-- bounds given are mostly O(1), but only if you are willing to accept
-- that the tree cannot have height greater than 7 on 32 bit systems
-- and maybe 8 on 64 bit systems.
--
-- TODO:
--
-- * More of the Data.Sequence API
--
-- * More efficient Eq and Ord instances.  This is tricky in the
--   presence of slicing.  There will be faster implementations for
--   unsliced inputs.
--
-- * Implement something to make parallel reductions simple (maybe
--   something like vector-strategies)
--
-- * Implement cons.  Cons can use the space that is hidden by the
--   offset cheaply.  It can also make a variant of pushTail
--   (pushHead) that allocates fragments of preceeding sub-trees.
--   Each cons call will modify the offset of its result vector.
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
module Data.Vector.Persistent (
  Vector,
  empty,
  null,
  length,
  singleton,
  index,
  unsafeIndex,
  snoc,
  update,
  (//),
  -- * Conversion
  fromList
  ) where

import Prelude hiding ( null, length, tail )

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
data Vector a = EmptyVector
              | RootNode { vecSize :: !Int
                         , vecShift :: !Int
                         , vecTail :: ![a]
                         , intVecPtrs :: !(Array (Vector a))
                         }
              | InternalNode { intVecPtrs :: !(Array (Vector a))
                             }
              | DataNode { dataVec :: !(Array a)
                         }
              deriving (Eq, Ord, Show)

instance Foldable Vector where
  foldr = pvFoldr
  foldl = pvFoldl

instance Functor Vector where
  fmap = pvFmap

instance Monoid (Vector a) where
  mempty = empty
  mappend = pvAppend

instance Traversable Vector where
  traverse = pvTraverse

instance (NFData a) => NFData (Vector a) where
  rnf = pvRnf

{-# INLINABLE pvFmap #-}
pvFmap :: (a -> b) -> Vector a -> Vector b
pvFmap f = go
  where
    go EmptyVector = EmptyVector
    go (DataNode v) = DataNode (A.map f v)
    go (InternalNode v) = InternalNode (A.map (fmap f) v)
    go (RootNode sz sh t v) =
      let t' = map f t
          v' = A.map (fmap f) v
      in RootNode sz sh t' v'

{-# INLINABLE pvFoldr #-}
pvFoldr :: (a -> b -> b) -> b -> Vector a -> b
pvFoldr f = go
  where
    go seed EmptyVector = seed
    go seed (DataNode a) = {-# SCC "gorDataNode" #-} A.foldr f seed a
    go seed (InternalNode as) = {-# SCC "gorInternalNode" #-}
      A.foldr (flip go) seed as
    go seed (RootNode _ _ t as) = {-# SCC "gorRootNode" #-}
      let tseed = F.foldl' (flip f) seed t
      in A.foldr (flip go) tseed as

{-# INLINABLE pvFoldl #-}
pvFoldl :: (b -> a -> b) -> b -> Vector a -> b
pvFoldl f = go
  where
    go seed EmptyVector = seed
    go seed (DataNode a) = {-# SCC "golDataNode" #-} A.foldl' f seed a
    go seed (InternalNode as) =
      A.foldl' go seed as
    go seed (RootNode _ _ t as) =
      let rseed = A.foldl' go seed as
      in F.foldr (flip f) rseed t

{-# INLINABLE pvTraverse #-}
pvTraverse :: (Applicative f) => (a -> f b) -> Vector a -> f (Vector b)
pvTraverse f = go
  where
    go EmptyVector = pure EmptyVector
    go (DataNode a) = DataNode <$> A.traverse f a
    go (InternalNode as) = InternalNode <$> A.traverse go as
    go (RootNode sz sh t as) =
      RootNode sz sh <$> T.traverse f t <*> A.traverse go as

{-# INLINABLE pvAppend #-}
pvAppend :: Vector a -> Vector a -> Vector a
pvAppend EmptyVector v = v
pvAppend v EmptyVector = v
pvAppend v1 v2 = F.foldl' snoc v1 (F.toList v2)

{-# INLINABLE pvRnf #-}
pvRnf :: (NFData a) => Vector a -> ()
pvRnf EmptyVector = ()
pvRnf (DataNode a) = rnf a
pvRnf (InternalNode a) = rnf a
pvRnf (RootNode _ _ t as) = rnf as `seq` rnf t

-- Functions

-- | The empty vector
empty :: Vector a
empty = EmptyVector

-- | Test to see if the vector is empty. (O(1))
null :: Vector a -> Bool
null EmptyVector = True
null _ = False

-- | Get the length of the vector. (O(1))
length :: Vector a -> Int
length EmptyVector = 0
length RootNode { vecSize = s } = s
length InternalNode {} = error "Data.Vector.Persistent.length: Internal nodes should not be exposed"
length DataNode {} = error "Data.Vector.Persistent.length: Data nodes should not be exposed"

-- | Bounds-checked indexing into a vector. (O(1))
index :: Vector a -> Int -> Maybe a
index v ix
  | length v > ix = Just $ unsafeIndex v ix
  | otherwise = Nothing

(!!#) :: [a] -> Int -> (# a #)
(!!#) [] !_ = error "(!!#): index out of range"
(!!#) (x : xs) i
  | i == 0 = (# x #)
  | otherwise = xs !!# (i - 1)

infixl 9 !!#

unsafeIndex :: Vector a -> Int -> a
unsafeIndex vec ix
  | (# a #) <- unsafeIndex# vec ix
  = a

-- | Unchecked indexing into a vector. (O(1))
--
-- Note that out-of-bounds indexing might not even crash - it will
-- usually just return nonsense values.
unsafeIndex# :: Vector a -> Int -> (# a #)
unsafeIndex# vec ix
  | ix >= tailOffset vec =
    reverse (vecTail vec) !!# (ix .&. 0x1f)
  | otherwise = go (vecShift vec) vec
  where
    go level v
      | level == 0 = A.index# (dataVec v) (ix .&. 0x1f)
      | otherwise =
        let nextVecIx = (ix `shiftR` level) .&. 0x1f
            v' = intVecPtrs v
        in go (level - 5) (A.index v' nextVecIx)

-- | Construct a vector with a single element. (O(1))
singleton :: a -> Vector a
singleton elt =
  RootNode { vecSize = 1
           , vecShift = 5
           , vecTail = [elt]
           , intVecPtrs = A.fromList 0 []
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
snoc EmptyVector elt = singleton elt
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
snoc _ _ = error "Data.Vector.Persistent.snoc: Internal nodes should not be exposed to the user"

pushTail :: Int -> [a] -> Int -> Array (Vector a) -> Array (Vector a)
pushTail cnt t = go
  where
    go level parent
      | level == 5 = arraySnoc parent (DataNode (A.fromList 32 (reverse t)))
      | subIdx < A.length parent =
        let nextVec = A.index parent subIdx
            toInsert = go (level - 5) (intVecPtrs nextVec)
        in A.update parent subIdx (InternalNode toInsert)
      | otherwise = arraySnoc parent (newPath (level - 5) t)
      where
        subIdx = ((cnt - 1) `shiftR` level) .&. 0x1f

newPath :: Int -> [a] -> Vector a
newPath level t
  | level == 0 = DataNode (A.fromList 32 (reverse t))
  | otherwise = InternalNode $ A.fromList 1 $ [newPath (level - 5) t]

-- | Update a single element at @ix@ with new value @elt@ in @v@. (O(1))
--
-- > update ix elt v
update :: Int -> a -> Vector a -> Vector a
update ix elt = (// [(ix, elt)])

-- | Bulk update.
--
-- > v // updates
--
-- For each (index, element) pair in @updates@, modify @v@ such that
-- the @index@th position of @v@ is @element@.
-- Indices in @updates@ that are not in @v@ are ignored
(//) :: Vector a -> [(Int, a)] -> Vector a
(//)  = foldr replaceElement

replaceElement :: (Int, a) -> Vector a -> Vector a
replaceElement _ EmptyVector = EmptyVector
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
          let rnode = go (level - 5) (intVecPtrs vec')
          in A.update vec vix (InternalNode rnode)
      where
        vix = (ix `shiftR` level) .&. 0x1f
        vec' = A.index vec vix
replaceElement _ _ = error "Data.Vector.Persistent.replaceElement: should not see internal nodes"

tailOffset :: Vector a -> Int
tailOffset v
  | len < 32 = 0
  | otherwise = (len - 1) `shiftR` 5 `shiftL` 5
  where
    len = length v

-- | Construct a vector from a list. (O(n))
fromList :: [a] -> Vector a
fromList = F.foldl' snoc empty
