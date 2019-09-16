-----------------------------------------------------------------------------
-- |
-- Module    : Utils.SBVTestFramework
-- Copyright : (c) Jeffrey Young
--                 Levent Erkok
-- License   : BSD3
-- Maintainer: erkokl@gmail.com
-- Stability : experimental
--
-- Various goodies for benchmarking SBV
-----------------------------------------------------------------------------

module Utils.SBVBenchFramework where

import qualified Data.List      as L
import           System.Process (showCommandForUser)

import qualified Data.SBV       as S

-- | make the string to call executable from the command line. All the heavy
-- lifting is done by 'System.Process.showCommandForUser', the rest is just
-- projections out of 'Data.SBV.SMTConfig'
mkExecString :: S.SMTConfig -> FilePath -> String
mkExecString config inputFile = showCommandForUser exec $ inputFile:opts
  where smtSolver = S.solver config
        exec      = S.executable smtSolver
        opts'     = S.options smtSolver config
        opts      = L.delete "-in" opts' -- remove opt for interactive mode so
                                         -- that this plays nice with
                                         -- criterion environments
