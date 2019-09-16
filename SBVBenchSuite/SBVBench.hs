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

import           BenchSuite.Batch.SBVOverhead

-- >>> S.satWith S.z3{S.transcript=Just "carcrash.txt"} (S.sBool "a")
-- <interactive>:655:2-10: error:
--     Not in scope: `S.satWith'
--     No module named `S' is imported.
-- <interactive>:655:12-15: error:
--     Not in scope: `S.z3'
--     No module named `S' is imported.
-- <interactive>:655:17-28: error:
--     Not in scope: `S.transcript'
--     No module named `S' is imported.
-- <interactive>:655:52-58: error:
--     Not in scope: `S.sBool'
--     No module named `S' is imported.

-- Our benchmark harness.
main :: IO ()
main = defaultMain [
  isolated
  ]
