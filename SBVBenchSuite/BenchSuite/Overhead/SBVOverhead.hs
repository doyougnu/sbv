-----------------------------------------------------------------------------
-- |
-- Module    : BenchSuite.Batch.SBVOverhead
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: erkokl@gmail.com
-- Stability : experimental
--
-- Assessing the overhead of calling solving examples via sbv vs individual solvers
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

module BenchSuite.Overhead.SBVOverhead
  ( mkSatOverheadBenchMark
  , mkPveOverheadBenchMark
  , mkOverheadBenchMark'
  , mkOverheadBenchMark
  ) where

import           Control.DeepSeq         (NFData(..), rwhnf)
import           System.Directory        (getCurrentDirectory)
import           System.IO

import           Criterion.Main

import qualified System.Process          as P
import qualified Utils.SBVBenchFramework as U

-- | The problem to benchmark, rendered as a file to input into solvers via the
-- shell and not sbv
type Problem = FilePath

-- | A bench unit is a solver and a problem that represents an input problem
-- for the solver to solve
type BenchUnit = (U.SMTConfig, Problem)

-- | A runner is either 'Data.SBV.proveWith' or 'Data.SBV.satWith'. Where "a" is
-- the problem sent to the solver and "b" is the return type. This could be a
-- type class but I don't expect these to change and this is pretty simple. We
-- require a runner in order to generate a 'Data.SBV.transcript' and then to run
-- the actual benchmark.
type Runner a b = (U.SMTConfig -> a -> IO b)

-- | Filepath to /dev/null
devNull :: FilePath
#ifndef WINDOWS
devNull = "/dev/null"
#else
devNull = "NUL"
#endif

-- | to bench a solver without interfacing through SBV we call transcript to
-- have SBV generate the input file for the solver and then create a process to
-- initiate execution on the solver. Note that we redirect stdout to /dev/devNull
-- or NUL on windows
runStandaloneSolver :: BenchUnit -> IO ()
runStandaloneSolver (slvr, fname) =
  withFile devNull WriteMode $
  (\h -> do (_,_,_,ph) <- P.createProcess (P.shell command){P.std_out = P.UseHandle h}
            _ <- P.waitForProcess ph
            return ())
  where command = U.mkExecString slvr fname

-- | Given a file name, a solver config, and a problem to solve, create an
-- environment for the criterion benchmark by generating a transcript file
standaloneEnv :: Runner a b -> U.SMTConfig -> a -> IO FilePath -> IO BenchUnit
standaloneEnv runner c problems f = f >>= go problems
  where
    -- for each symbolic bench unit, generate a transcript for the unit
    -- go :: U.Provable a => a -> FilePath -> IO BenchUnit
    go p file = do pwd <- getCurrentDirectory
                   let fPath = mconcat [pwd,"/",file]
                   _ <- runner c{U.transcript = Just fPath} p >> return ()
                   return (c,fPath)

-- | Cleanup the environment created by criterion by removing the transcript
-- file used to run the standalone solver
standaloneCleanup :: BenchUnit -> IO ()
standaloneCleanup (_,fPath) =  P.callCommand $ "rm " ++ fPath

-- | To construct a benchmark to test SBV's overhead we setup an environment
-- with criterion where a symbolic computation is emitted to a transcript file.
-- To test the solver without respect to SBV (standalone) we pass the transcript
-- file to the solver using the same primitives SBV does. Not that mkFileName
-- generates a random filename that is removed at the end of the benchmark. This
-- function exposes the solver and the solve interface in case the user would
-- like to benchmark with something other than 'Data.SBV.z3' and so that we can
-- benchmark all solving variants, e.g., 'Data.SBV.proveWith',
-- 'Data.SBV.satWith', 'Data.SBV.allProveWith' etc.
mkOverheadBenchMark' :: NFData b => Runner a b -> U.SMTConfig -> String -> a -> Benchmark
mkOverheadBenchMark' runner slvr desc problem =
  envWithCleanup
  (standaloneEnv runner slvr problem U.mkFileName)
  standaloneCleanup $
  \ ~unit ->
    bgroup desc [ bench "standalone" $ nfIO $ runStandaloneSolver unit
                -- notice for sbv benchmark; we pull the solver out of unit and
                -- use the input problem not the transcript in the unit
                , bench "sbv"        $ nfIO $ runner (fst unit) problem
                ]

-- | Convenience functions for benchmarking.
mkSatOverheadBenchMark :: U.Provable a => String -> a -> Benchmark
mkSatOverheadBenchMark = mkOverheadBenchMark' U.satWith U.z3

mkPveOverheadBenchMark :: U.Provable a => String -> a -> Benchmark
mkPveOverheadBenchMark = mkOverheadBenchMark' U.proveWith U.z3

mkOverheadBenchMark :: (U.Provable a, NFData b) => Runner a b -> String -> a -> Benchmark
mkOverheadBenchMark = flip mkOverheadBenchMark' U.z3

-- | Orphaned instances just for benchmarking
instance NFData U.AllSatResult where
  rnf (U.AllSatResult (a, b, c, results)) =
    rnf a `seq` rnf b `seq` rnf c `seq` rwhnf results