module Test.ActiveUTxOs.UpdatePaymentAddress where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.UpdatePaymentAddress.Regressions qualified as Regressions
import Test.ActiveUTxOs.UpdatePaymentAddress.Failures qualified as Failures
import Test.ActiveUTxOs.UpdatePaymentAddress.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Updating Payment Addresses" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
