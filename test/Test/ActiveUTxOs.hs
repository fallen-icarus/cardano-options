module Test.ActiveUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.PurchaseContract qualified as PurchaseContract
import Test.ActiveUTxOs.UpdatePaymentAddress qualified as UpdatePaymentAddress
import Test.ActiveUTxOs.ExecuteContract qualified as ExecuteContract
import Test.ActiveUTxOs.CloseExpiredContract qualified as CloseExpiredContract
import Test.ActiveUTxOs.Misc qualified as Misc

tests :: TestTree
tests = testGroup "Active UTxO Tests"
  [ PurchaseContract.tests
  , UpdatePaymentAddress.tests
  , ExecuteContract.tests
  , CloseExpiredContract.tests
  , Misc.tests
  ]
