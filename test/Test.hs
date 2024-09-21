module Main where

import Test.Tasty

import Test.ActiveUTxOs qualified as ActiveUTxOs
import Test.Beacons qualified as Beacons
import Test.ProposalUTxOs qualified as ProposalUTxOs
import Test.Misc qualified as Misc

main :: IO ()
main = defaultMain $ testGroup "Cardano-Options"
  [ ActiveUTxOs.tests
  , ProposalUTxOs.tests
  , Beacons.tests
  , Misc.tests
  ]
