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

-- # OPTIONS_GHC -Wall -Werror #
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module BenchSuite.Batch.SBVOverhead where

import           Control.Monad                               (zipWithM)
import           System.Process                              (callCommand)
import           System.Directory                            (getCurrentDirectory)
import           System.IO

import           Criterion.Main

import qualified Data.SBV                                    as S
import qualified Documentation.SBV.Examples.Puzzles.Birthday as D
import qualified System.Process                              as P
import           Utils.SBVBenchFramework

-- | The problem to benchmark, rendered as a file to input into solvers via the
-- shell and not sbv
type Problem = FilePath

-- | A bench unit is a solver and a problem that represents an input problem
-- for the solver to solve
type BenchUnit = (S.SMTConfig, Problem)

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
benchStandaloneSolver :: BenchUnit -> IO ()
benchStandaloneSolver (solver, fname) =
  withFile devNull WriteMode $
  (\h -> do (_,_,_,ph) <- P.createProcess (P.shell command){P.std_out = P.UseHandle h}
            _ <- P.waitForProcess ph
            return ())
  where command = mkExecString solver fname


-- | Given a file name, a solver config, and a problem to solve, create an
-- environment for the criterion benchmark by generating a transcript file
standaloneEnv :: S.Provable a => S.SMTConfig -> [FilePath] -> [a] -> IO [BenchUnit]
standaloneEnv solver fs problems = zipWithM go fs problems
  where
    go :: S.Provable a => FilePath -> a -> IO BenchUnit
    go f p = do pwd <- getCurrentDirectory
                let fPath = mconcat [pwd,"/",f]
                _ <- S.satWith solver{S.transcript = Just fPath} p >> return ()
                return (solver,fPath)

standaloneCleanup :: [BenchUnit] -> IO ()
standaloneCleanup bs = mapM_ step bs
  where step (_,fPath) = P.callCommand $ "rm " ++ fPath

exampleBench :: Benchmark
exampleBench = bench "test" $ nfIO $ S.sat D.puzzle

isolated :: Benchmark
isolated = envWithCleanup
  (standaloneEnv S.z3 ["testfile"] [D.puzzle])
  standaloneCleanup $ \ ~units ->
                        bgroup "standalone" [ bench "test" $ nfIO $ benchStandaloneSolver (head units)
                                            , exampleBench
                                            ]
