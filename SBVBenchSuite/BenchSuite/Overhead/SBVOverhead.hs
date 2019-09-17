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

{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

module BenchSuite.Overhead.SBVOverhead
  ( mkOverheadBenchMark
  , mkOverheadBenchMark'
  ) where

import           System.Directory        (getCurrentDirectory)
import           System.IO

import           Criterion.Main

import qualified Data.SBV                as S
import qualified System.Process          as P
import           Utils.SBVBenchFramework

-- | The problem to benchmark, rendered as a file to input into solvers via the
-- shell and not sbv
type Problem = FilePath

-- | A bench unit is a solver and a problem that represents an input problem
-- for the solver to solve
type BenchUnit = (S.SMTConfig, Problem)

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
  where command = mkExecString slvr fname

-- | Given a file name, a solver config, and a problem to solve, create an
-- environment for the criterion benchmark by generating a transcript file
standaloneEnv :: S.Provable a => S.SMTConfig -> a -> IO FilePath -> IO BenchUnit
standaloneEnv slvr problems f = f >>= go problems
  where
    -- for each symbolic bench unit, generate a transcript for the unit
    go :: S.Provable a => a -> FilePath -> IO BenchUnit
    go p file = do pwd <- getCurrentDirectory
                   let fPath = mconcat [pwd,"/",file]
                   _ <- S.satWith slvr{S.transcript = Just fPath} p >> return ()
                   return (slvr,fPath)

-- | Cleanup the environment created by criterion by removing the transcript
-- file used to run the standalone solver
standaloneCleanup :: BenchUnit -> IO ()
standaloneCleanup (_,fPath) =  P.callCommand $ "rm " ++ fPath

-- | To construct a benchmark to test SBV's overhead we setup an environment
-- with criterion where a symbolic computation is emitted to a transcript file.
-- To test the solver without respect to SBV (standalone) we pass the transcript
-- file to the solver using the same primitives SBV does. Not that mkFileName
-- generates a random filename that is removed at the end of the benchmark. This
-- version exposes the solver in case the user would like to benchmark with
-- something other than 'Data.SBV.z3'
mkOverheadBenchMark' :: S.Provable a => S.SMTConfig -> String -> a -> Benchmark
mkOverheadBenchMark' slvr desc problem =
  envWithCleanup
  (standaloneEnv slvr problem mkFileName)
  standaloneCleanup $
  \ ~unit ->
    bgroup desc [ bench "standalone" $ nfIO $ runStandaloneSolver unit
                -- notice for sbv benchmark; we pull the solver out of unit and
                -- use the input problem not the transcript in the unit
                , bench "sbv"        $ nfIO $ S.satWith (fst unit) problem
                ]

-- | To construct a benchmark to test SBV's overhead we setup an environment
-- with criterion where a symbolic computation is emitted to a transcript file.
-- To test the solver without respect to SBV (standalone) we pass the transcript
-- file to the solver using the same primitives SBV does. Not that mkFileName
-- generates a random filename that is removed at the end of the benchmark
mkOverheadBenchMark :: S.Provable a => String -> a -> Benchmark
mkOverheadBenchMark = mkOverheadBenchMark' S.z3
