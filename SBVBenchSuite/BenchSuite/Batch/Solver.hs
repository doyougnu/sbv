-----------------------------------------------------------------------------
-- |
-- Module    : Utils.SBVTestFramework
-- Copyright : (c) Jeffrey Young
--                 Levent Erkok
-- License   : BSD3
-- Maintainer: erkokl@gmail.com
-- Stability : experimental
--
-- Module that runs solvers without relying on Data.SBV.SMT.SMT
-----------------------------------------------------------------------------

import Data.SBV
import Documentation.SBV.Examples.Lists.Fibonacci

import System.Cmd