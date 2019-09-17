-----------------------------------------------------------------------------
-- |
-- Module    : BenchSuite.Puzzles.SendMoreMoney
-- Copyright : (c) Jeffrey Young
--                 Levent Erkok
-- License   : BSD3
-- Maintainer: erkokl@gmail.com
-- Stability : experimental
--
-- Bench suite for Documentation.SBV.Examples.Puzzles.SendMoreMoney
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

module BenchSuite.Puzzles.SendMoreMoney(benchmarks) where


import Utils.SBVBenchFramework
import BenchSuite.Overhead.SBVOverhead


-- benchmark suite
benchmarks :: Benchmark
benchmarks = mkOverheadBenchMark "Puzzles.SendMoreMoney" p
  where p = do
          ds@[s,e,n,d,m,o,r,y] <- mapM sInteger ["s", "e", "n", "d", "m", "o", "r", "y"]
          let isDigit x = x .>= 0 .&& x .<= 9
              val xs    = sum $ zipWith (*) (reverse xs) (iterate (*10) 1)
              send      = val [s,e,n,d]
              more      = val [m,o,r,e]
              money     = val [m,o,n,e,y]
          constrain $ sAll isDigit ds
          constrain $ distinct ds
          constrain $ s ./= 0 .&& m ./= 0
          solve [send + more .== money]
