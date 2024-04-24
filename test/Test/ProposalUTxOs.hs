module Test.ProposalUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.ProposalUTxOs.CreateProposal qualified as CreateProposal

tests :: TestTree
tests = testGroup "Proposal UTxO Tests"
  [ CreateProposal.tests
  ]
