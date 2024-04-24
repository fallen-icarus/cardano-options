module Test.ActiveUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.PurchaseContract qualified as PurchaseContract

tests :: TestTree
tests = testGroup "Active UTxO Tests"
  [ PurchaseContract.tests
  ]
