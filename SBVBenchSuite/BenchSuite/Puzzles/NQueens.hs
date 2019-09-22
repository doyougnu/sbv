-----------------------------------------------------------------------------
-- |
-- Module    : BenchSuite.Puzzles.NQueens
-- Copyright : (c) Jeffrey Young
--                 Levent Erkok
-- License   : BSD3
-- Maintainer: erkokl@gmail.com
-- Stability : experimental
--
-- Bench suite for Documentation.SBV.Examples.Puzzles.NQueens
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

module BenchSuite.Puzzles.NQueens(benchmarks) where

import Documentation.SBV.Examples.Puzzles.NQueens

import Utils.SBVBenchFramework
import BenchSuite.Overhead.SBVOverhead


-- benchmark suite
benchmarks :: Benchmark
benchmarks = bgroup "Puzzles.NQueens"
  [ mkOverheadBenchMark allSatWith "NQueens 1" (mkQueens 1)
  , mkOverheadBenchMark allSatWith "NQueens 2" (mkQueens 2)
  , mkOverheadBenchMark allSatWith "NQueens 3" (mkQueens 3)
  , mkOverheadBenchMark allSatWith "NQueens 4" (mkQueens 4)
  , mkOverheadBenchMark allSatWith "NQueens 5" (mkQueens 5)
  , mkOverheadBenchMark allSatWith "NQueens 6" (mkQueens 6)
  , mkOverheadBenchMark allSatWith "NQueens 7" (mkQueens 7)
  , mkOverheadBenchMark allSatWith "NQueens 8" (mkQueens 8)
  ]

mkQueens :: Int -> Symbolic SBool
mkQueens n = isValid n `fmap` mkExistVars n