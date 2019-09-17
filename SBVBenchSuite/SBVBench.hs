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


module Main where

import           Criterion.Main

import qualified BenchSuite.Puzzles.Coins
import qualified BenchSuite.Puzzles.NQueens

-- The bench harness
main :: IO ()
main = defaultMain [ -- BenchSuite.Puzzles.Coins.benchmarks
                     BenchSuite.Puzzles.NQueens.benchmarks
                   ]
