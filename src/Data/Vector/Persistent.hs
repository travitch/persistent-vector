{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneDeriving #-}
{-# language ViewPatterns #-}
{- OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}
{- options_ghc -ddump-simpl -ddump-rule-rewrites #-}

-- | This is a port of the persistent vector from clojure to Haskell.
-- It is spine-strict and lazy in the elements.
--
-- The implementation is based on array mapped tries.  The complexity
-- bounds given are mostly O(1), but only if you are willing to accept
-- that the tree cannot have height greater than 7 on 32 bit systems
-- and maybe 8 on 64 bit systems.
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- options_ghc -ddump-simpl #-}

module Data.Vector.Persistent (
  Vector,
  -- * Construction
  empty,
  singleton,
  snoc,
  fromList,
  append,
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
  foldr',
  foldl,
  foldl',
  -- * Transformations
  map,
  reverse,
  -- * Searches
  filter,
  partition,
  valid
  ) where

import Prelude hiding
  ( null, length, tail, take
  , drop, map, foldr, foldl
  , reverse, splitAt, filter )

import qualified Control.Applicative as Ap
import Control.Exception (assert)
import Control.DeepSeq
import Data.Bits hiding (shiftR, shiftL)
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Semigroup as Sem
import qualified Data.Traversable as T

import Data.Vector.Persistent.Array ( Array )
import Data.Vector.Persistent.Level
import qualified Data.Vector.Persistent.Array as A
import Data.Type.Equality
#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif

-- Note: using Int here doesn't give the full range of 32 bits on a 32
-- bit machine (it is fine on 64)

-- | Persistent vectors based on array mapped tries
data Vector a
  = forall lev. RootNode
     { vecSize :: !Int
     , vecShift :: !(SLevel lev)
     , vecTail :: ![a]
     , intVecPtrs :: !(Vector_ lev a)
     }

deriving instance Show a => Show (Vector a)

data Vector_ lev a where
  InternalNode ::
      { intVecPtrs_ :: !(Array (Vector_ lev a))
      } -> Vector_ ('FivePlus lev) a
  DataNode ::
      { dataVec :: !(Array a)
      } -> Vector_ 'Zero a

deriving instance Show a => Show (Vector_ lev a)

instance Eq a => Eq (Vector a) where
  (==) = pvEq

instance Eq a => Eq (Vector_ lev a) where
  (==) = pvEq_

instance Ord a => Ord (Vector a) where
  compare = pvCompare

instance Ord a => Ord (Vector_ lev a) where
  compare = pvCompare_

instance F.Foldable Vector where
  foldMap = T.foldMapDefault
  foldr = foldr
  foldl = foldl
#if MIN_VERSION_base(4,6,0)
  foldr' = foldr'
  foldl' = foldl'
#endif
#if MIN_VERSION_base(4,8,0)
  length = length
  null = null
#endif

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

instance NFData a => NFData (Vector_ lev a) where
  rnf = pvRnf_

shiftR :: Int -> Int -> Int
{-# INLINE shiftR #-}
shiftR = unsafeShiftR

shiftL :: Int -> Int -> Int
{-# INLINE shiftL #-}
shiftL = unsafeShiftL

{-# INLINABLE pvEq #-}
pvEq :: Eq a => Vector a -> Vector a -> Bool
pvEq (RootNode sz1 sh1 t1 v1) (RootNode sz2 sh2 t2 v2) =
  sz1 == sz2 && (sz1 == 0 || proceed)
  where
    proceed
      | Just Refl <- testEquality sh1 sh2
      = t1 == t2 && v1 == v2
      | otherwise
      = error "pvEq: Trees of the same size should have the same depth."

{-# INLINABLE pvEq_ #-}
pvEq_ :: Eq a => Vector_ lev a -> Vector_ lev a -> Bool
pvEq_ (DataNode a1) (DataNode a2) = a1 == a2
pvEq_ (InternalNode a1) (InternalNode a2) = a1 == a2

{-# INLINABLE pvCompare #-}
pvCompare :: Ord a => Vector a -> Vector a -> Ordering
pvCompare (RootNode sz1 sh1 t1 v1) (RootNode sz2 sh2 t2 v2) =
  compare sz1 sz2 <> case testEquality sh1 sh2 of
    Just Refl -> if sz1 == 0 then EQ else compare v1 v2 <> compare t1 t2
    Nothing -> error "pvCompare: Trees of the same size should have the same depth."

{-# INLINABLE pvCompare_ #-}
pvCompare_ :: Ord a => Vector_ lev a -> Vector_ lev a -> Ordering
pvCompare_ (DataNode a1) (DataNode a2) = compare a1 a2
pvCompare_ (InternalNode a1) (InternalNode a2) = compare a1 a2

{-# INLINABLE map #-}
-- | \( O(n) \) Map over the vector
map :: forall a b. (a -> b) -> Vector a -> Vector b
map f = go
  where
    go (RootNode sz sh t v) =
      let t' = L.map f t
          v' = go_ v
      in RootNode sz sh t' v'

    go_ :: Vector_ lev a -> Vector_ lev b
    go_ (DataNode v) = DataNode (A.map f v)
    go_ (InternalNode v) = InternalNode (A.map go_ v)

{-# INLINE foldr #-}
-- | \( O(n) \) Right fold over the vector
foldr :: forall a b. (a -> b -> b) -> b -> Vector a -> b
foldr f = go
  where
    go seed (RootNode _ _ t as) = {-# SCC "gorRootNode" #-}
      let tseed = F.foldr f seed (L.reverse t)
      in go_ as tseed
    go_ :: Vector_ lev a -> b -> b
    go_ (DataNode a) seed = {-# SCC "gorDataNode" #-} A.foldr f seed a
    go_ (InternalNode as) seed = {-# SCC "gorInternalNode" #-}
      A.foldr go_ seed as

-- | \( O(n) \) Strict right fold over the vector.
--
-- Note: Strict in the initial accumulator value.
foldr' :: forall a b. (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr' #-}
foldr' f = go
  where
    go !seed (RootNode _ _ t as) = {-# SCC "gorRootNode" #-}
      let !tseed = F.foldl' (flip f) seed t
      in go_ as tseed
    go_ :: Vector_ lev a -> b -> b
    go_ (DataNode a) !seed = {-# SCC "gorDataNode" #-} A.foldr' f seed a
    go_ (InternalNode as) !seed = {-# SCC "gorInternalNode" #-}
      A.foldr' go_ seed as

-- | \( O(n) \) Left fold over the vector.
foldl :: forall b a. (b -> a -> b) -> b -> Vector a -> b
{-# INLINE foldl #-}
foldl f = go
  where
    go_ :: b -> Vector_ lev a -> b
    go seed (RootNode _ _ t as) =
      let rseed = go_ seed as
      in F.foldr (flip f) rseed t

    go_ seed (DataNode a) = {-# SCC "golDataNode" #-} A.foldl f seed a
    go_ seed (InternalNode as) =
      A.foldl go_ seed as

-- | \( O(n) \) Strict left fold over the vector.
--
-- Note: Strict in the initial accumulator value.
foldl' :: forall b a. (b -> a -> b) -> b -> Vector a -> b
{-# INLINE foldl' #-}
foldl' f = go
  where
    go !seed (RootNode _ _ t as) =
      let !rseed = go_ seed as
      in F.foldl' f rseed (L.reverse t)
    go_ :: b -> Vector_ lev a -> b
    go_ !seed (DataNode a) = {-# SCC "golDataNode" #-} A.foldl' f seed a
    go_ !seed (InternalNode as) =
      A.foldl' go_ seed as

{-# INLINE pvTraverse #-}
pvTraverse :: forall f a b. Ap.Applicative f => (a -> f b) -> Vector a -> f (Vector b)
pvTraverse f = go
  where
    go (RootNode sz sh t as)
      | sz == 0 = Ap.pure empty
      | otherwise = Ap.liftA2 (RootNode sz sh) (T.traverse f t) (go_ as)
    go_ :: Vector_ lev a -> f (Vector_ lev b)
    go_ (DataNode a) = DataNode Ap.<$> A.traverseArray f a
    go_ (InternalNode as) = InternalNode Ap.<$> A.traverseArray go_ as

append :: Vector a -> Vector a -> Vector a
append v1 v2
  | null v1 = v2
  | null v2 = v1
append v1 v2 = F.foldl' snoc v1 v2

{-# INLINABLE pvRnf #-}
pvRnf :: NFData a => Vector a -> ()
pvRnf (RootNode _ _ t as) = rnf as `seq` rnf t

{-# INLINABLE pvRnf_ #-}
pvRnf_ :: NFData a => Vector_ lev a -> ()
pvRnf_ (DataNode a) = rnf a
pvRnf_ (InternalNode a) = rnf a

-- | \( O(1) \) The empty vector.
empty :: Vector a
empty = RootNode 0 SZero [] (DataNode A.empty)

-- | \( O(1) \) Test to see if the vector is empty.
null :: Vector a -> Bool
null xs = length xs == 0

-- | \( O(1) \) Get the length of the vector.
length :: Vector a -> Int
length RootNode { vecSize = s } = s

-- | \( O(1) \) Bounds-checked indexing into a vector.
index :: Vector a -> Int -> Maybe a
index v ix
  -- Check if the index is valid. This funny business uses a single test to
  -- determine whether ix is too small (negative) or too large (at least the
  -- length of the vector).
  | (fromIntegral ix :: Word) < fromIntegral (length v)
  = unsafeIndexA v ix
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

-- | \( O(1) \) Unchecked indexing into a vector.
--
-- Out-of-bounds indexing might not even crash—it will
-- usually just return nonsense values.
--
-- Note: the actual lookup is not performed until the result is forced.
-- This can cause a memory leak if the result of indexing is stored, unforced,
-- after the rest of the vector becomes garbage. To avoid this, use
-- 'unsafeIndexA' or 'unsafeIndex#' instead.
unsafeIndex :: Vector a -> Int -> a
unsafeIndex vec ix
  | (# a #) <- unsafeIndex# vec ix
  = a

-- | \( O(1) \) Unchecked indexing into a vector in the context of an arbitrary
-- 'Ap.Applicative' functor. If the 'Ap.Applicative' is "strict" (such as 'IO',
-- (strict) @ST s@, (strict) @StateT@, or 'Maybe', but not @Identity@,
-- @ReaderT@, etc.), then the lookup is performed before the next action. This
-- avoids space leaks that can result from lazy uses of 'unsafeIndex'. See the
-- documentation for 'unsafeIndex#' for a custom 'Ap.Applicative' that can be
-- especially useful in conjunction with this function.
--
-- Note that out-of-bounds indexing might not even crash—it will usually just
-- return nonsense values.
unsafeIndexA :: Ap.Applicative f => Vector a -> Int -> f a
{-# INLINABLE unsafeIndexA #-}
unsafeIndexA vec ix
  | (# a #) <- unsafeIndex# vec ix
  = Ap.pure a

-- | \( O(1) \) Unchecked indexing into a vector.
--
-- Note that out-of-bounds indexing might not even crash—it will
-- usually just return nonsense values.
--
-- This function exists mostly because there is not, as yet, a well-known,
-- canonical, and convenient /lifted/ unary tuple. So we instead offer an
-- eager indexing function returning an /unlifted/ unary tuple. Users who
-- prefer to avoid such "low-level" features can do something like this:
--
-- @
-- data Solo a = Solo a deriving Functor
-- instance Applicative Solo where
--   pure = Solo
--   liftA2 f (Solo a) (Solo b) = Solo (f a b)
-- @
--
-- Now
--
-- @
-- unsafeIndexA :: Vector a -> Int -> Solo a
-- @
unsafeIndex# :: Vector a -> Int -> (# a #)
unsafeIndex# vec@RootNode{vecShift = sh, intVecPtrs = tree} ix
  | ix >= tailOffset vec =
    (vecTail vec) `revIx#` (ix .&. 0x1f)
  | otherwise =
      go sh tree
  where
    go :: SLevel lev -> Vector_ lev a -> (# a #)
    go SZero (DataNode v) = A.index# v (ix .&. 0x1f)
    go level@(SFivePlus level') (InternalNode v) =
        let nextVecIx = (ix `shiftR` unSLevel level) .&. 0x1f
        in go level' (A.index v nextVecIx)

-- | \( O(1) \) Construct a vector with a single element.
singleton :: a -> Vector a
singleton elt =
  RootNode { vecSize = 1
           , vecShift = SZero
           , vecTail = [elt]
           , intVecPtrs = DataNode A.empty
           }

-- | A helper to copy an array and add an element to the end.
arraySnoc :: Array a -> a -> Array a
arraySnoc !a elt = A.run $ do
  let alen = A.length a
  a' <- A.new_ (1 + alen)
  A.copy a 0 a' 0 alen
  A.write a' alen elt
  return a'

-- | \( O(1) \) Append an element to the end of the vector.
snoc :: Vector a -> a -> Vector a
-- We break this up into two pieces. We let the common case inline:
-- that is the case of a nonempty vector with room in its tail.
-- The case of an empty vector isn't common enough to bother inlining,
-- and the case of a full tail is expensive anyway, so there's nothing
-- to be gained by inlining. The remaining question: do we actually
-- benefit from letting *any* of this inline? To be determined, but my
-- guess is a strong maybe.
snoc v@RootNode { vecSize = sz, vecTail = t } elt
  -- Room in tail, and vector non-empty
  | sz .&. 0x1f /= 0 = v { vecTail = elt : t, vecSize = sz + 1 }
  | otherwise = snocMain v elt

snocMain :: forall a. Vector a -> a -> Vector a
{-# NOINLINE snocMain #-}
snocMain v@RootNode { vecSize = sz, vecShift = sh, intVecPtrs = tree, vecTail = t } elt
  -- Overflow current root
  | sz `shiftR` 5 > 1 `shiftL` unSLevel sh
  = let !np = newPath sh tvec
    in finish (SFivePlus sh)
              (InternalNode $ A.pair tree np)
  | otherwise
  = case sh of
      SZero
        | null v
        -> singleton elt
        | otherwise
        -> assert (sz == 32) $ finish sh tvec
      SFivePlus _
        -- Insert into the tree
        ->  finish sh (pushTail sz tvec sh tree)
  where
    !tvec = DataNode (A.fromListRev 32 t)
    finish :: SLevel lev -> Vector_ lev a -> Vector a
    finish sh' ptrs' = RootNode
      { vecSize = sz + 1
      , vecShift = sh'
      , vecTail = [elt]
      , intVecPtrs = ptrs' }

-- | A recursive helper for 'snoc'.  This finds the place to add new
-- elements.
--
-- Must be called only with an 'InternalNode'.
pushTail :: forall lev a. Int -> Vector_ 'Zero a -> SLevel ('FivePlus lev) -> Vector_ ('FivePlus lev) a -> Vector_ ('FivePlus lev) a
pushTail !cnt !t !foo !bar = go foo bar
  where
    go :: forall lev'. SLevel ('FivePlus lev') -> Vector_ ('FivePlus lev') a -> Vector_ ('FivePlus lev') a
    -- Note: This outer pattern match doesn't actually perform
    -- an equality test; that's sorted out by a rewrite rule in the
    -- Level implementation.
    --
    -- GHC does a really lousy job with this function if we use pattern
    -- synonyms here. The rewrite rule doesn't fire, so we get unnecessary
    -- equality tests. Worse, a pattern match failure continuation doesn't
    -- get recognized as a join point. So we just use 'checkFive' manually
    -- here.
    go level@(checkFive -> IsFivePlus (lev' :: SLevel m)) (InternalNode parent)
      | subIdx < A.length parent
        -- The IsFivePlus test is guaranteed to succeed here; we need
        -- it to get the types right in the recursive call. I don't think
        -- this is likely to be a big performance problem; if I'm wrong
        -- we can `unsafeCoerce` our way to glory.
      , IsFivePlus _ <- checkFive lev' =
        let
            nextVec :: Vector_ m a
            nextVec = A.index parent subIdx
            toInsert :: Vector_ m a
            toInsert = go lev' nextVec
        in InternalNode $ A.update parent subIdx $! toInsert
      | otherwise = InternalNode $ arraySnoc parent $! newPath lev' t
      where
        subIdx = ((cnt - 1) `shiftR` unSLevel level) .&. 0x1f

-- | The other recursive helper for 'snoc'.  This one builds out a
-- sub-tree to the current depth.
newPath :: SLevel lev -> Vector_ 'Zero a -> Vector_ lev a
newPath SZero t = t
newPath (SFivePlus lev') t = InternalNode $ A.singleton $! newPath lev' t

-- | Update a single element at @ix@ with new value @elt@.
updateList :: Int -> a -> [a] -> [a]
-- We do this pretty strictly to avoid building up thunks in the tail
-- and to release the replaced value promptly.
updateList !_ _ [] = []
updateList 0 x (_ : ys) = x : ys
updateList n x (y : ys) = (y :) $! updateList (n - 1) x ys

-- | \( O(1) \) Update a single element at @ix@ with new value @elt@ in @v@.
--
-- > update ix elt v
update :: forall a. Int -> a -> Vector a -> Vector a
update ix elt vec@(RootNode { vecShift = sh, vecSize = sz, intVecPtrs = tree, vecTail = t })
  -- Invalid index. This funny business uses a single test to determine whether
  -- ix is too small (negative) or too large (at least sz).
  | (fromIntegral ix :: Word) >= fromIntegral sz = vec
  -- Item is in tail
  | ix >= tailOffset vec =
    let tix = sz - 1 - ix
    in vec { vecTail = updateList tix elt t}
  -- Otherwise the item to be replaced is in the tree
  | otherwise =
      RootNode {intVecPtrs = go sh tree, vecShift = sh, vecTail = t, vecSize = sz}
  where
    go :: SLevel lev -> Vector_ lev a -> Vector_ lev a
    go SZero (DataNode v) = DataNode $ A.update v (ix .&. 0x1f) elt
    go level@(SFivePlus lev') (InternalNode v) =
        let nextVecIx = (ix `shiftR` unSLevel level) .&. 0x1f
        in InternalNode $ A.updateWith v nextVecIx $! go lev'

-- | \( O(n) \) Bulk update.
--
-- > v // updates
--
-- For each @(index, element)@ pair in @updates@, modify @v@ such that
-- the @index@th position of @v@ is @element@.
-- Indices in @updates@ that are not in @v@ are ignored. The updates are
-- applied in order, so the last one at each index takes effegct.
(//) :: Vector a -> [(Int, a)] -> Vector a
-- Note: we fully apply foldl' to get everything to unbox.
(//) vec = L.foldl' replaceElement vec
  where
    replaceElement v (ix, a) = update ix a v

-- | The index of the first element of the tail of the vector (that is, the
-- *last* element of the list representing the tail). This is also the number
-- of elements stored in the array tree.
--
-- Caution: Only gives a sensible result if the vector is nonempty.
tailOffset :: Vector a -> Int
tailOffset v = (length v - 1) .&. ((-1) `shiftL` 5)

-- | \( O(n) \) Reverse a vector
reverse :: Vector a -> Vector a
{-# NOINLINE reverse #-}
reverse = foldr' (flip snoc) empty

-- | \( O(n) \) Filter according to the predicate.
filter :: (a -> Bool) -> Vector a -> Vector a
filter p = \ !vec -> foldl' go empty vec
  where
    go !acc e = if p e then snoc acc e else acc

-- | \( O(n) \) Return the elements that do and do not obey the predicate
partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
partition p v0 = case F.foldl' go (TwoVec empty empty) v0 of
  TwoVec v1 v2 -> (v1, v2)
  where
    go (TwoVec atrue afalse) e =
      if p e then TwoVec (snoc atrue e) afalse else TwoVec atrue (snoc afalse e)

data TwoVec a = TwoVec {-# UNPACK #-} !(Vector a) {-# UNPACK #-} !(Vector a)

-- | \( O(n) \) Construct a vector from a list. (O(n))
fromList :: [a] -> Vector a
fromList = F.foldl' snoc empty

-- | Does the vector have an acceptable shape?
valid :: Vector a -> Bool
valid v@RootNode { vecSize = sz, intVecPtrs = tree, vecTail = t }
  | sz == 0
  = L.null t && case tree of
      DataNode vec -> A.length vec == 0
      InternalNode _ -> False
  | sz <= 32
  = case tree of
      DataNode vec -> A.length vec == 0
      InternalNode _ -> False
  | L.null t = False
  | L.length t > 32 = False
  | sz /= foldr' (\_ acc -> acc + 1) 0 v = False
  | Invalid <- valid_ tree
  = False
  | otherwise = True

-- Checks that a subtree is valid.
valid_ :: Vector_ lev a -> Validity
valid_ (DataNode v)
  | A.length v == 32
  = NoBranches
  | otherwise = Invalid
valid_ (InternalNode as)
  | 1 <= len && len <= 32
  = F.foldMap full_ (L.init (A.toList as)) <> valid_ (A.index as (len - 1))
  | otherwise = Invalid
  where len = A.length as

-- Returns Invalid if the tree is not full
full_ :: Vector_ lev a -> Validity
full_ (DataNode as)
  | A.length as == 32
  = NoBranches
  | otherwise = Invalid
full_ (InternalNode as)
  | A.length as == 32
  = F.foldMap full_ (A.toList as)
  | otherwise = Invalid

data Validity = Invalid | Branches | NoBranches
instance Semigroup Validity where
  Invalid <> _ = Invalid
  _ <> Invalid = Invalid
  NoBranches <> NoBranches = NoBranches
  _ <> _ = Branches

instance Monoid Validity where
  mempty = NoBranches
  mappend = (<>)
