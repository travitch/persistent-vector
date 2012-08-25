module Main ( main ) where

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck

import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Traversable as T

import Data.Vector.Persistent ( Vector )
import qualified Data.Vector.Persistent as V

newtype InputList = InputList [Int]
                  deriving (Show)
instance Arbitrary InputList where
  arbitrary = sized inputList

inputList :: Int -> Gen InputList
inputList sz = do
  modifier <- choose (0, 1000)
  l <- vector (sz * modifier)
  return $ InputList l

tests :: [Test]
tests = [ testProperty "toListFromListIdent" prop_toListFromListIdentity
        , testProperty "foldrWorks" prop_foldrWorks
        , testProperty "foldlWorks" prop_foldlWorks
        , testProperty "mappendWorks" prop_mappendWorks
        ]

main :: IO ()
main = defaultMain tests

prop_toListFromListIdentity :: InputList -> Bool
prop_toListFromListIdentity (InputList il) =
  il == F.toList (V.fromList il)

prop_foldrWorks :: InputList -> Bool
prop_foldrWorks (InputList il) =
  F.foldr (+) 0 il == F.foldr (+) 0 (V.fromList il)

prop_foldlWorks :: InputList -> Bool
prop_foldlWorks (InputList il) =
  F.foldl' (flip (:)) [] il == F.foldl (flip (:)) [] (V.fromList il)

prop_mappendWorks :: (InputList, InputList) -> Bool
prop_mappendWorks (InputList il1, InputList il2) =
  (il1 `mappend` il2) == F.toList (V.fromList il1 `mappend` V.fromList il2)