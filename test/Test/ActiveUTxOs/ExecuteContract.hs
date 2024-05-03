module Test.ActiveUTxOs.ExecuteContract where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.ExecuteContract.Regressions qualified as Regressions
import Test.ActiveUTxOs.ExecuteContract.Failures qualified as Failures
import Test.ActiveUTxOs.ExecuteContract.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Executing Contracts" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
