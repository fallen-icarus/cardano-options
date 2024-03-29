module Main where

import Test.Tasty

import Test.CreateAssetsUTxO as CreateAssetsUTxO
import Test.ProposeContracts as ProposeContracts
import Test.CloseAssetsUTxO as CloseAssetsUTxO
import Test.CloseProposalUTxO as CloseProposalUTxO
import Test.AcceptContract as AcceptContract
import Test.ExecuteContract as ExecuteContract
import Test.CloseExpiredContract as CloseExpiredContract
import Test.UpdateAddress as UpdateAddress

main :: IO ()
main = defaultMain $ testGroup "Cardano-Options"
  [
    CreateAssetsUTxO.tests
  , ProposeContracts.tests
  , CloseAssetsUTxO.tests
  , CloseProposalUTxO.tests
  , AcceptContract.tests
  , ExecuteContract.tests
  , CloseExpiredContract.tests
  , UpdateAddress.tests
  ]