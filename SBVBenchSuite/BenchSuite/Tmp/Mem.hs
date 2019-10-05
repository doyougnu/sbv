-----------------------------------------------------------------------------
-- |
-- Module    : BenchSuite.Puzzles.Birthday
-- Copyright : (c) Jeffrey Young
--                 Levent Erkok
-- License   : BSD3
-- Maintainer: erkokl@gmail.com
-- Stability : experimental
--
-- Bench suite for Documentation.SBV.Examples.Puzzles.Birthday
-----------------------------------------------------------------------------

module BenchSuite.Tmp.Mem (benchmarks) where

import Documentation.SBV.Examples.Puzzles.Birthday

import Utils.SBVBenchFramework
import BenchSuite.Overhead.SBVOverhead

import Control.Monad
import Data.SBV
-- import Criterion.Main

-- program taken from
-- https://github.com/LeventErkok/sbv/issues/460#issuecomment-469590832

bad x = sat $ do bs <- mapM (const free_) [1..x]
                 foldM go sTrue bs
  where go x acc = let s = x .&& acc
                   in do constrain s
                         return s

good x = sat $ do bs <- mapM (const free_) [1..x]
                  foldM go sTrue bs
  where go x acc = return $ x .&& acc

-- main = do print =<< length . show <$> bad
--           print =<< length . show <$> good

benchmarks =
       [
         bgroup "Good" [ bench "10"  . nfIO $ good 10
                       , bench "100"  . nfIO $ good 100
                       , bench "1000"  . nfIO $ good 1000
                       -- , bench "10000"  . nfIO $ good 10000
                       ]
       ,
         bgroup "Bad" [ bench "10"  . nfIO $ bad 10
                      , bench "100"  . nfIO $ bad 100
                      , bench "1000"  . nfIO $ bad 1000
                      -- , bench "10000"  . nfIO $ bad 10000
                      ]
       ]

-- -- benchmark suite
-- benchmarks :: Benchmark
-- benchmarks = runner "Birthday" puzzle `using` setRunner allSatWith
