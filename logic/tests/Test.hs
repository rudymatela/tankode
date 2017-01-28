-- | This module defines utilities to test 'Speculate' itself.
--
-- It should never be exported in a cabal package, and should not be included
-- in Haddock documentation.  Hence the weird name, simply "Test".
--
-- This module exports a Listable Expr instance, that does not, by any means,
-- list all possible expressions.  But instead, list expressions based on the
-- names exported by this module.
module Test
  ( reportTests
  , getMaxTestsFromArgs
  , mainTest 

  , module Test.LeanCheck
  , module Test.LeanCheck.Utils
  )
where

import Test.LeanCheck
import Test.LeanCheck.Utils

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (elemIndices)

import Tankode
import Geometry

reportTests :: [Bool] -> IO ()
reportTests tests =
  case elemIndices False tests of
    [] -> putStrLn "+++ Tests passed!"
    is -> do putStrLn ("*** Failed tests:" ++ show is)
             exitFailure

getMaxTestsFromArgs :: Int -> IO Int
getMaxTestsFromArgs n = do
  as <- getArgs
  return $ case as of
             (s:_) -> read s
             _     -> n

mainTest :: (Int -> [Bool]) -> Int -> IO ()
mainTest tests n' = do
  n <- getMaxTestsFromArgs n'
  reportTests (tests n)
