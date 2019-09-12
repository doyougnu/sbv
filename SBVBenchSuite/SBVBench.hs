-----------------------------------------------------------------------------
-- |
-- Module    : SBVBench
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: erkokl@gmail.com
-- Stability : experimental
--
-- Main entry point to the bench suite
-----------------------------------------------------------------------------


module Main(main) where

import Criterion.Main
import BenchSuite.Batch.SBVOverhead


-- Our benchmark harness.
main :: IO ()
main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "9"  $ whnf fib 9
               , bench "11" $ whnf fib 11
               ]
  ]
