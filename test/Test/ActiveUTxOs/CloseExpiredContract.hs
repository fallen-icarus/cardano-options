module Test.ActiveUTxOs.CloseExpiredContract where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.CloseExpiredContract.Regressions qualified as Regressions
import Test.ActiveUTxOs.CloseExpiredContract.Failures qualified as Failures
import Test.ActiveUTxOs.CloseExpiredContract.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Close Expired Contracts" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
