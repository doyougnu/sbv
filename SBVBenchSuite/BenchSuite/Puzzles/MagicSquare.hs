-----------------------------------------------------------------------------
-- |
-- Module    : BenchSuite.Puzzles.MagicSquare
-- Copyright : (c) Jeffrey Young
--                 Levent Erkok
-- License   : BSD3
-- Maintainer: erkokl@gmail.com
-- Stability : experimental
--
-- Bench suite for Documentation.SBV.Examples.Puzzles.MagicSquare
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

module BenchSuite.Puzzles.MagicSquare(benchmarks) where

import Documentation.SBV.Examples.Puzzles.MagicSquare

import Utils.SBVBenchFramework
import BenchSuite.Overhead.SBVOverhead


-- benchmark suite
benchmarks :: Benchmark
benchmarks = bgroup "MagicSquare"
  [ mkOverheadBenchMark allSatWith "magic 2" (mkMagic 2)
  , mkOverheadBenchMark allSatWith "magic 3" (mkMagic 3)
  ]

mkMagic :: Int -> Symbolic SBool
mkMagic n = (isMagic . chunk n) `fmap` mkExistVars (n*n)