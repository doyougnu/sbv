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

module BenchSuite.Batch.SBVOverhead where

-- import Data.SBV

-- The function we're benchmarking.
fib :: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)
