-----------------------------------------------------------------------------
-- |
-- Module    : SBVBench
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: erkokl@gmail.com
-- Stability : experimental
--
-- Main entry point to the bench suite.
-----------------------------------------------------------------------------

module Main where

import           Criterion.Main
import           Criterion.Types (Config(..))

import BenchSuite.Overhead.SBVOverhead

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

-- | Custom config to limit benchmarks to 5 minutes of runtime. This is required
-- because we can easily generate benchmarks that take a lot of wall time to
-- solve, especially with 'Data.SBV.allSatWith' calls
benchConfig :: Config
benchConfig = defaultConfig {timeLimit = 300.00}

-- The bench harness
main :: IO ()
main = defaultMainWith benchConfig $
       [ puzzles
       ]

-- | Benchmarks for 'Documentation.SBV.Examples.Puzzles'. This looks a little
-- wonky but the redundant solution works well in this situation. Each benchmark
-- file defines a 'benchmarks' function which returns a type parameterized by
-- two type variables. These type variables describe the input and the output of
-- the solver calls. We want to hold on to this type information for as long as
-- possible because it allows us to manipulate the benchmark before running the
-- benchmark. Thus we only want to generate the 'Benchmark' type in this file.
-- The main benefit out doing so is that now we can define the benchmark
-- mirroring the logic of the symbolic program. But that might be expensive to
-- benchmark, so using this method we can change solver details _without_
-- redefining the benchmark, as I have done below by converting all the
-- 'Data.SBV.allSat' examples to a single 'Data.SBV.satWith' call.
puzzles :: Benchmark
puzzles = bgroup "Puzzles" $ mkOverheadBenchMark <$> 
          [ BenchSuite.Puzzles.Coins.benchmarks
          , BenchSuite.Puzzles.Counts.benchmarks
          , BenchSuite.Puzzles.Birthday.benchmarks
          , BenchSuite.Puzzles.DogCatMouse.benchmarks
          , BenchSuite.Puzzles.Euler185.benchmarks
          , BenchSuite.Puzzles.Garden.benchmarks
          , BenchSuite.Puzzles.LadyAndTigers.benchmarks
          , BenchSuite.Puzzles.SendMoreMoney.benchmarks
          , BenchSuite.Puzzles.NQueens.benchmarks
          , BenchSuite.Puzzles.MagicSquare.benchmarks
          , BenchSuite.Puzzles.Sudoku.benchmarks
          , BenchSuite.Puzzles.U2Bridge.benchmarks
          ]
