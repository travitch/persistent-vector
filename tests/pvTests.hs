module Main ( main ) where

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck

import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.List as L
import qualified Data.Traversable as T

import Data.Vector.Persistent ( Vector )
import qualified Data.Vector.Persistent as V

newtype InputList = InputList [Int]
                  deriving (Show)
instance Arbitrary InputList where
  arbitrary = sized inputList

data IndexableList = IndexableList [Int] Int
                   deriving (Show)

instance Arbitrary IndexableList where
  arbitrary = sized indexableList

data SliceList = SliceList [Int] Int Int
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

indexableList :: Int -> Gen IndexableList
indexableList sz = do
  modifier <- choose (0, 100)
  l <- vector (1 + (sz * modifier))
  ix <- choose (0, length l - 1)
  return $ IndexableList l ix

inputList :: Int -> Gen InputList
inputList sz = do
  modifier <- choose (0, 100)
  l <- vector (sz * modifier)
  return $ InputList l

tests :: [Test]
tests = [ testProperty "toListFromListIdent" prop_toListFromListIdentity
        , testProperty "fmap" prop_map
        , testProperty "foldrWorks" prop_foldrWorks
        , testProperty "foldlWorks" prop_foldlWorks
        , testProperty "updateWorks" prop_updateWorks
        , testProperty "indexingWorks" prop_indexingWorks
        , testProperty "take" prop_take
        , testProperty "drop" prop_drop
        , testProperty "splitAt" prop_splitAt
        , testProperty "slice" prop_slice
        , testProperty "slicedFoldl'" prop_slicedFoldl'
        , testProperty "slicedFoldr" prop_sliceFoldr
        , testProperty "mappendWorks" prop_mappendWorks
        , testProperty "shrink" prop_shrinkPreserves
        , testProperty "shrinkEq" prop_shrinkEquality
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
  F.foldl' (flip (:)) [] il == V.foldl' (flip (:)) [] (V.fromList il)

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

prop_indexingWorks :: IndexableList -> Bool
prop_indexingWorks (IndexableList il ix) =
  (il !! ix) == (V.unsafeIndex (V.fromList il) ix)

prop_take :: IndexableList -> Bool
prop_take (IndexableList il ix) =
  L.take ix il == F.toList (V.take ix (V.fromList il))

prop_drop :: IndexableList -> Bool
prop_drop (IndexableList il ix) =
  L.drop ix il == F.toList (V.drop ix (V.fromList il))

prop_splitAt :: IndexableList -> Bool
prop_splitAt (IndexableList il ix) =
  let (v1, v2) = V.splitAt ix (V.fromList il)
  in L.splitAt ix il == (F.toList v1, F.toList v2)

listSlice :: Int -> Int -> [a] -> [a]
listSlice s n = L.take n . (L.drop s)

prop_slice :: SliceList -> Bool
prop_slice (SliceList il s n) =
  listSlice s n il == F.toList (V.slice s n (V.fromList il))

prop_sliceFoldr :: SliceList -> Bool
prop_sliceFoldr (SliceList il s n) =
  L.foldr (:) [] (listSlice s n il) == V.foldr (:) [] (V.slice s n (V.fromList il))

prop_slicedFoldl' :: SliceList -> Bool
prop_slicedFoldl' (SliceList il s n) =
  L.foldl' (flip (:)) [] (listSlice s n il) == V.foldl' (flip (:)) [] (V.slice s n (V.fromList il))

prop_mappendWorks :: (InputList, InputList) -> Bool
prop_mappendWorks (InputList il1, InputList il2) =
  (il1 `mappend` il2) == F.toList (V.fromList il1 `mappend` V.fromList il2)

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