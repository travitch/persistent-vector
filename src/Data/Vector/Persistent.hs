module Data.Vector.Persistent (
  Vector,
  empty,
  null,
  length,
  singleton,
  index,
  unsafeIndex,
  snoc,
  -- * Conversion
  fromList
  ) where

-- TODO: It would be great to have unboxed versions of this using
-- unboxed vectors.  Obviously there is no need for mutable versions
-- as in Vector.

import Prelude hiding ( null, length, tail )

import Data.Bits
import Data.Foldable ( Foldable )
import qualified Data.Foldable as F
import qualified Data.Vector as V

-- Note: using Int here doesn't give the full range of 32 bits on a 32
-- bit machine (it is fine on 64)
data Vector a = EmptyVector
              | RootNode { vecSize :: !Int
                         , vecShift :: !Int
                         , vecTail :: !(V.Vector a)
                         , intVecPtrs :: !(V.Vector (Vector a))
                         }
              | InternalNode { intVecPtrs :: !(V.Vector (Vector a))
                             }
              | DataNode { dataVec :: !(V.Vector a)
                         }
              deriving (Eq, Ord, Show)

-- instance Foldable (Vector a) where
--   F.foldr = pvFoldr

-- pvFoldr :: (a -> b -> b) -> b -> Vector a -> b
-- pvFoldr _ seed EmptyVector = seed
-- pvFoldr f seed (RootNode _ shift tail internal) =
--   go (intVecPtrs internal) `f` go
--   where
--     go acc v =

empty :: Vector a
empty = EmptyVector

null :: Vector a -> Bool
null EmptyVector = True
null _ = False

length :: Vector a -> Int
length EmptyVector = 0
length RootNode { vecSize = s } = s
length InternalNode {} = error "Internal nodes should not be exposed"
length DataNode {} = error "Data nodes should not be exposed"

index :: Vector a -> Int -> Maybe a
index v ix
  | length v > ix = Just $ unsafeIndex v ix
  | otherwise = Nothing

unsafeIndex :: Vector a -> Int -> a
unsafeIndex vec ix
  | ix >= tailOffset vec =
    V.unsafeIndex (vecTail vec) (ix .&. 0x1f)
  | otherwise = go (fromIntegral (vecShift vec)) vec
  where
    wordIx = fromIntegral ix
    go level v
      | level == 0 = V.unsafeIndex (dataVec v) (wordIx .&. 0x1f)
      | otherwise =
        let nextVecIx = (wordIx `shiftR` level) .&. 0x1f
            v' = intVecPtrs v
        in go (level - 5) (V.unsafeIndex v' nextVecIx)

singleton :: a -> Vector a
singleton elt =
  RootNode { vecSize = 1
           , vecShift = 5
           , vecTail = V.singleton elt
           , intVecPtrs = V.empty
           }

snoc :: Vector a -> a -> Vector a
snoc EmptyVector elt = singleton elt
snoc v elt
  -- If there is room in the tail, add the new element there
  | not (nodeIsFull (vecTail v)) = v { vecTail = V.snoc (vecTail v) elt
                                  , vecSize = vecSize v + 1
                                  }
  -- Otherwise, insert the tail into the tree and add a new tail with
  -- this element
  | otherwise =
    case pushTail (vecShift v - 5) (intVecPtrs v) (vecTail v) of
      -- In this case, the root really has been modified to add the
      -- tail into the tree structure
      (newRoot, Nothing) ->
        RootNode { vecSize = vecSize v + 1
                 , vecTail = V.singleton elt
                 , vecShift = vecShift v
                 , intVecPtrs = newRoot
                 }
      -- Adding a new level; the oldRoot is really the entire old
      -- subtree unmodified and we start a new subtree next to it
      (oldRoot, Just expansion) ->
        RootNode { vecSize = vecSize v + 1
                 , vecShift = vecShift v + 5
                 , vecTail = V.singleton elt
                 , intVecPtrs = V.fromList [ InternalNode oldRoot
                                           , InternalNode expansion
                                           ]
                 }


pushTail :: Int -> V.Vector (Vector a) -> V.Vector a -> (V.Vector (Vector a), Maybe (V.Vector (Vector a)))
pushTail level arr tail
  | level == 0 =
    case nodeIsFull arr of
      True -> (arr, Just (V.singleton (DataNode tail)))
      False -> (V.snoc arr (DataNode tail), Nothing)
  | otherwise =
      let nextArr = intVecPtrs (V.last arr)
      in case pushTail (level - 5) nextArr tail of
        (newChild, Nothing) ->
          (arr V.// [(V.length arr - 1, InternalNode newChild)], Nothing)
        (_, Just newChild) ->
          case nodeIsFull arr of
            True -> (arr, Just newChild)
            False -> (V.snoc arr (InternalNode newChild), Nothing)

tailOffset :: Vector a -> Int
tailOffset v
  | len < 32 = 0
  | otherwise = (len - 1) `shiftR` 5 `shiftL` 5
  where
    len = length v

nodeIsFull :: V.Vector a -> Bool
nodeIsFull = (==32) . V.length

fromList :: [a] -> Vector a
fromList = F.foldl' snoc empty
