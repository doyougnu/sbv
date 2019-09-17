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
import qualified BenchSuite.Puzzles.Counts
import qualified BenchSuite.Puzzles.Birthday
import qualified BenchSuite.Puzzles.DogCatMouse
import qualified BenchSuite.Puzzles.Euler185
import qualified BenchSuite.Puzzles.MagicSquare
import qualified BenchSuite.Puzzles.Garden
import qualified BenchSuite.Puzzles.LadyAndTigers
import qualified BenchSuite.Puzzles.SendMoreMoney
import qualified BenchSuite.Puzzles.Sudoku
import qualified BenchSuite.Puzzles.U2Bridge

-- The bench harness
main :: IO ()
main = defaultMain [ puzzles
                   ]

puzzles :: Benchmark
puzzles = bgroup "Puzzles" [ BenchSuite.Puzzles.Coins.benchmarks
                           , BenchSuite.Puzzles.NQueens.benchmarks
                           , BenchSuite.Puzzles.Counts.benchmarks
                           , BenchSuite.Puzzles.Birthday.benchmarks
                           , BenchSuite.Puzzles.DogCatMouse.benchmarks
                           , BenchSuite.Puzzles.Euler185.benchmarks
                           , BenchSuite.Puzzles.MagicSquare.benchmarks
                           , BenchSuite.Puzzles.Garden.benchmarks
                           , BenchSuite.Puzzles.LadyAndTigers.benchmarks
                           , BenchSuite.Puzzles.SendMoreMoney.benchmarks
                           , BenchSuite.Puzzles.Sudoku.benchmarks
                           , BenchSuite.Puzzles.U2Bridge.benchmarks
                           ]
