module Main ( main ) where

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck

import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.List as L

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

{-
-- This doesn't actually seem like a gr
data IndexableList = IndexableList [Int] !Int
  deriving Show

instance Arbitrary IndexableList where
  arbitrary = sized indexableList

indexableList :: Int -> Gen IndexableList
indexableList sz = do
  len <- chooseInt (1, max 1 (sz))
  IndexableList <$> vector len <*> chooseInt (0, len - 1)
-}


sizedList :: Int -> Gen SizedList
sizedList sz = do
  len <- chooseInt (0, sz)
  lst <- vector len
  pure $ SizedList lst len

inputList :: Int -> Gen InputList
inputList sz = do
  len <- chooseInt (1, max 1 (sz)) -- Tune this
  InputList <$> vector len

tests :: [Test]
tests = [ testProperty "toListFromListIdent" prop_toListFromListIdentity
        , testProperty "fmap" prop_map
        , testProperty "foldrWorks" prop_foldrWorks
        , testProperty "foldlWorks" prop_foldlWorks
        , testProperty "updateWorks" prop_updateWorks
        , testProperty "indexingWorks" prop_indexingWorks
        , testProperty "mappendWorks" prop_mappendWorks
        , testProperty "eqWorksEqual" prop_eqWorks_equal
        , testProperty "eqWorks" prop_eqWorks
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
  (il1 `mappend` il2) == F.toList (V.fromList il1 <> V.fromList il2)

prop_eqWorks_equal :: InputList -> Bool
prop_eqWorks_equal (InputList il) =
  V.fromList il == V.fromList il

prop_eqWorks :: InputList -> InputList -> Bool
prop_eqWorks (InputList il1) (InputList il2) =
  (V.fromList il1 == V.fromList il2) == (il1 == il2)
