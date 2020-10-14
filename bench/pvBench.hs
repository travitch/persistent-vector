module Main ( main ) where

import Criterion.Main

import Control.DeepSeq
import qualified Data.Foldable as F
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Sequence ( Seq )
import qualified Data.Sequence as S
import Data.Vector.Persistent ( Vector )
import qualified Data.Vector.Persistent as V

testListRTraversal :: [Int] -> Int
testListRTraversal = F.foldr (+) 0

testListLSTraversal :: [Int] -> Int
testListLSTraversal = F.foldl' (+) 0

testVecRTraversal :: Vector Int -> Int
testVecRTraversal = F.foldr (+) 0

testIMRTraversal :: IntMap Int -> Int
testIMRTraversal = F.foldr (+) 0

testVecLTraversal :: Vector Int -> Int
testVecLTraversal = F.foldl (+) 0

testSeqRTraversal :: Seq Int -> Int
testSeqRTraversal = F.foldr (+) 0

testSeqLTraversal :: Seq Int -> Int
testSeqLTraversal = F.foldl (+) 0

testVecLSTraversal :: Vector Int -> Int
testVecLSTraversal = F.foldl' (+) 0

testSeqLSTraversal :: Seq Int -> Int
testSeqLSTraversal = F.foldl' (+) 0

testListIndex :: [Int] -> Int -> Int
testListIndex = (!!)

testSeqIndex :: Seq Int -> Int -> Int
testSeqIndex = S.index

testVecIndex :: Vector Int -> Int -> Int
testVecIndex = V.unsafeIndex

testIMIndex :: IntMap Int -> Int -> Maybe Int
testIMIndex = flip IM.lookup

testSeqRange :: Seq Int -> Int -> Int -> Int
testSeqRange s start end = sum $ map (S.index s) [start..end]

testVecRange :: Vector Int -> Int -> Int -> Int
testVecRange v start end = sum $ map (V.unsafeIndex v) [start..end]

testIMAppend :: IntMap Int -> Int -> Int -> IntMap Int
testIMAppend im ix i = IM.insert ix i im

testSeqAppend :: Seq Int -> Int -> Seq Int
testSeqAppend = (S.|>)

testVecAppend :: Vector Int -> Int -> Vector Int
testVecAppend = V.snoc

testIMUpdate :: IntMap Int -> Int -> Int -> IntMap Int
testIMUpdate m ix elt = IM.insert ix elt m

testSeqUpdate :: Seq Int -> Int -> Int -> Seq Int
testSeqUpdate s ix elt = S.update ix elt s

testVecUpdate :: Vector Int -> Int -> Int -> Vector Int
testVecUpdate v ix elt = V.update ix elt v

testIMBuild :: Int -> IntMap Int
testIMBuild len =
  let l = [0..len]
  in L.foldl' (\m (ix, elt) -> IM.insert ix elt m) IM.empty (zip l l)

testSeqBuild :: Int -> Seq Int
testSeqBuild len = L.foldl' (S.|>) S.empty [0..len]

testVecBuild :: Int -> Vector Int
testVecBuild len = L.foldl' V.snoc V.empty [0..len]

main :: IO ()
main = setup >> defaultMain [
  bgroup "traverse1000" [ bench "ListR1000" $ whnf testListRTraversal l1000
                        , bench "IMR1000" $ whnf testIMRTraversal im1000
                        , bench "SeqR1000" $ whnf testSeqRTraversal s1000
                        , bench "VecR1000" $ whnf testVecRTraversal v1000
                        , bench "SeqL1000" $ whnf testSeqLTraversal s1000
                        , bench "VecL1000" $ whnf testVecLTraversal v1000
                        , bench "ListLS1000" $ whnf testListLSTraversal l1000
                        , bench "SeqLS1000" $ whnf testSeqLSTraversal s1000
                        , bench "VecLS1000" $ whnf testVecLSTraversal v1000
                        ],
  bgroup "indexAt3_1000" [ bench "List1000" $ whnf (testListIndex l1000) 3
                         , bench "IM1000" $ whnf (testIMIndex im1000) 3
                         , bench "Seq1000" $ whnf (testSeqIndex s1000) 3
                         , bench "Vec1000" $ whnf (testVecIndex v1000) 3
                         ],
  bgroup "indexAt30_1000" [ bench "List1000" $ whnf (testListIndex l1000) 30
                          , bench "IM1000" $ whnf (testIMIndex im1000) 30
                          , bench "Seq1000" $ whnf (testSeqIndex s1000) 30
                          , bench "Vec1000" $ whnf (testVecIndex v1000) 30
                          ],
  bgroup "indexAt30_10000" [ bench "Seq10000" $ whnf (testSeqIndex s10000) 30
                           , bench "Vec10000" $ whnf (testVecIndex v10000) 30
                           ],
  bgroup "indexAt3000_10000" [ bench "IM1000" $ whnf (testIMIndex im10000) 3000
                             , bench "Seq10000" $ whnf (testSeqIndex s10000) 3000
                             , bench "Vec10000" $ whnf (testVecIndex v10000) 3000
                             ],
  bgroup "indexRange501-550_10000" [ bench "Seq10000" $ whnf (testSeqRange s10000 501) 550
                                   , bench "Vec10000" $ whnf (testVecRange v10000 501) 550
                                   ],
  bgroup "indexRange5001-5500_100000" [ bench "Seq10000" $ whnf (testSeqRange s100000 5001) 5500
                                      , bench "Vec10000" $ whnf (testVecRange v100000 5001) 5500
                                      ],
  bgroup "appendValue1000" [ bench "IM1000" $ whnf (testIMAppend im1000 1000) 1
                           , bench "Seq1000" $ whnf (testSeqAppend s1000) 1
                           , bench "Vec1000" $ whnf (testVecAppend v1000) 1
                           ],
  bgroup "appendValue100000" [ bench "IM100000" $ whnf (testIMAppend im100000 100000) 1
                             , bench "Seq100000" $ whnf (testSeqAppend s100000) 1
                             , bench "Vec100000" $ whnf (testVecAppend v100000) 1
                             ],
  bgroup "updateIndex5_1000" [ bench "IM1000" $ whnf (testIMUpdate im1000 5) 342
                             , bench "Seq1000" $ whnf (testSeqUpdate s1000 5) 342
                             , bench "Vec1000" $ whnf (testVecUpdate v1000 5) 342
                             ],
  bgroup "updateIndex5000_100000" [ bench "IM100000" $ whnf (testIMUpdate im100000 5000) 342
                                  , bench "Seq100000" $ whnf (testSeqUpdate s100000 5000) 342
                                  , bench "Vec100000" $ whnf (testVecUpdate v100000 5000) 342
                                  ],
  bgroup "build1000" [ bench "IM1000" $ whnf testIMBuild 1000
                     , bench "Seq1000" $ whnf testSeqBuild 1000
                     , bench "Vec1000" $ whnf testVecBuild 1000
                     ]
  ]
  where
    setup = do
      l1000 `deepseq` l10000 `deepseq` l10000 `deepseq`
        v1000 `deepseq` v10000 `deepseq` v100000 `deepseq`
        im1000 `deepseq` im10000 `deepseq` im100000 `deepseq` return ()
      S.sort s1000 `seq` S.sort s10000 `seq` S.sort s100000 `seq` return ()
    l1000 :: [Int]
    l1000 = [0..1000]
    l10000 :: [Int]
    l10000 = [0..10000]
    l100000 :: [Int]
    l100000 = [0..100000]
    v1000 = V.fromList l1000
    v10000 = V.fromList l10000
    v100000 = V.fromList l100000
    s1000 = S.fromList l1000
    s10000 = S.fromList l10000
    s100000 = S.fromList l100000
    im1000 = IM.fromList (zip l1000 l1000)
    im10000 = IM.fromList (zip l10000 l10000)
    im100000 = IM.fromList (zip l100000 l100000)
