module Test.ActiveUTxOs.PurchaseContract where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.PurchaseContract.Regressions qualified as Regressions
import Test.ActiveUTxOs.PurchaseContract.Failures qualified as Failures
import Test.ActiveUTxOs.PurchaseContract.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Purchasing Contracts" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
