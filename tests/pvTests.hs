{-# LANGUAGE CPP #-}
module Main ( main ) where

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck

import qualified Data.Foldable as F
import qualified Data.Monoid as DM
import qualified Data.List as L
import qualified Control.Applicative as Ap
import qualified Data.Traversable as T

--import Data.Vector.Persistent ( Vector )
import qualified Data.Vector.Persistent as V

newtype InputList = InputList [Int]
                  deriving (Show)
instance Arbitrary InputList where
  arbitrary = sized inputList

data SizedList = SizedList [Int] !Int
  deriving Show

instance Arbitrary SizedList where
  arbitrary = sized sizedList

sizedList :: Int -> Gen SizedList
sizedList sz = do
  len <- chooseInt (0, sz)
  lst <- vector len
  return $ SizedList lst len

inputList :: Int -> Gen InputList
inputList sz = do
  len <- chooseInt (0, max 1 sz)
  InputList Ap.<$> vector len

tests :: [Test]
tests = [ testProperty "toListFromListIdent" prop_toListFromListIdentity
        , testProperty "fmap" prop_map
        , testProperty "foldrWorks" prop_foldrWorks
        , testProperty "foldlWorks" prop_foldlWorks
        , testProperty "traverseWorks" prop_traverseWorks
        , testProperty "updateWorks" prop_updateWorks
        , testProperty "indexingWorks" prop_indexingWorks
        , testProperty "mappendWorks" prop_mappendWorks
        , testProperty "eqWorksEqual" prop_eqWorks_equal
        , testProperty "eqWorks" prop_eqWorks
        , testProperty "shrink" prop_shrinkPreserves
        , testProperty "shrinkEq" prop_shrinkEquality
        , testProperty "appendAfterSlice" prop_appendAfterSlice
        , testProperty "takeWhile" prop_takeWhile
        , testProperty "dropWhile" prop_dropWhile
        , testProperty "take" prop_take
        , testProperty "drop" prop_drop
        , testProperty "splitAt" prop_splitAt
        , testProperty "slice" prop_slice
        , testProperty "slicedFoldl'" prop_slicedFoldl'
        , testProperty "slicedFoldr" prop_sliceFoldr
        ]

main :: IO ()
main = defaultMain tests

prop_toListFromListIdentity :: InputList -> Bool
prop_toListFromListIdentity (InputList il) =
  il == F.toList (V.fromList il)

prop_map :: InputList -> Bool
prop_map (InputList il) =
  L.map f il == F.toList (fmap f (V.fromList il))
  where
    f = (+20)

prop_foldrWorks :: InputList -> Bool
prop_foldrWorks (InputList il) =
  F.foldr (+) 0 il == F.foldr (+) 0 (V.fromList il)

prop_foldlWorks :: InputList -> Bool
prop_foldlWorks (InputList il) =
  F.foldl (flip (:)) [] il == F.foldl (flip (:)) [] (V.fromList il)

prop_updateWorks :: (InputList, Int, Int) -> Property
prop_updateWorks (InputList il, ix, repl) =
  ix >= 0 ==> rlist == F.toList (v V.// [(ix, repl)])
  where
    v = V.fromList il
    (keepHead, _:keepTail) = L.splitAt ix il
    rlist = case null il of
      True -> []
      False -> case ix >= length il of
        True -> il
        False -> keepHead ++ (repl : keepTail)

prop_indexingWorks :: SizedList -> Bool
prop_indexingWorks (SizedList il sz) =
  il == [V.unsafeIndex vec ix | ix <- [0..sz - 1]]
  where
    vec = V.fromList il

prop_mappendWorks :: (InputList, InputList) -> Bool
prop_mappendWorks (InputList il1, InputList il2) =
  (il1 `DM.mappend` il2) == F.toList (V.fromList il1 `DM.mappend` V.fromList il2)

prop_eqWorks_equal :: InputList -> Bool
prop_eqWorks_equal (InputList il) =
  V.fromList il == V.fromList il

prop_eqWorks :: InputList -> InputList -> Bool
prop_eqWorks (InputList il1) (InputList il2) =
  (V.fromList il1 == V.fromList il2) == (il1 == il2)

prop_traverseWorks :: InputList -> Bool
prop_traverseWorks (InputList il) =
  fmap F.toList (T.traverse go (V.fromList il)) == T.traverse go il
  where
    go a = ([a], a)

prop_take :: SizedList -> Bool
prop_take (SizedList il ix) =
  L.take ix il == F.toList (V.take ix (V.fromList il))

prop_drop :: SizedList -> Bool
prop_drop (SizedList il ix) =
  L.drop ix il == F.toList (V.drop ix (V.fromList il))

prop_splitAt :: SizedList -> Bool
prop_splitAt (SizedList il ix) =
  let (v1, v2) = V.splitAt ix (V.fromList il)
  in L.splitAt ix il == (F.toList v1, F.toList v2)

listSlice :: Int -> Int -> [a] -> [a]
listSlice s n = L.take n . (L.drop s)

data SliceList = SliceList [Int] !Int !Int
               deriving (Show)

instance Arbitrary SliceList where
  arbitrary = sized sliceList

sliceList :: Int -> Gen SliceList
sliceList sz = do
  modifier <- choose (0, 100)
  l <- vector (1 + (sz * modifier))
  start <- choose (0, length l - 1)
  len <- choose (0, 100)
  return $ SliceList l start len

prop_slice :: SliceList -> Bool
prop_slice (SliceList il s n) =
  listSlice s n il == F.toList (V.slice s n (V.fromList il))

prop_sliceFoldr :: SliceList -> Bool
prop_sliceFoldr (SliceList il s n) =
  L.foldr (:) [] (listSlice s n il) == V.foldr (:) [] (V.slice s n (V.fromList il))

prop_slicedFoldl' :: SliceList -> Bool
prop_slicedFoldl' (SliceList il s n) =
  L.foldl' (flip (:)) [] (listSlice s n il) == V.foldl' (flip (:)) [] (V.slice s n (V.fromList il))

prop_shrinkPreserves :: SliceList -> Bool
prop_shrinkPreserves (SliceList il s n) =
  F.toList v0 == F.toList (V.shrink v0)
  where
    v0 = V.slice s n (V.fromList il)

prop_shrinkEquality :: SliceList -> Bool
prop_shrinkEquality (SliceList il s n) =
  v0 == V.shrink v0
  where
    v0 = V.slice s n (V.fromList il)

prop_appendAfterSlice :: (SliceList, Int) -> Property
prop_appendAfterSlice (SliceList il s n, elt) =
  n - s < L.length il ==> Just elt == V.index v1 (V.length v1 - 1)
  where
    v0 = V.slice s n (V.fromList il)
    v1 = V.snoc v0 elt

prop_takeWhile :: SizedList -> Bool
prop_takeWhile (SizedList il ix) =
  L.takeWhile (<ix) il == F.toList v
  where
    v = V.takeWhile (<ix) $ V.fromList il

prop_dropWhile :: SizedList -> Bool
prop_dropWhile (SizedList il ix) =
  L.dropWhile (<ix) il == F.toList v
  where
    v = V.dropWhile (<ix) $ V.fromList il
