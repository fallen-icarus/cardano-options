module Main where

import Test.Tasty

import Test.ActiveUTxOs qualified as ActiveUTxOs
import Test.ProposalUTxOs qualified as ProposalUTxOs

main :: IO ()
main = defaultMain $ testGroup "Cardano-Options"
  [ ActiveUTxOs.tests
  , ProposalUTxOs.tests
  ]
