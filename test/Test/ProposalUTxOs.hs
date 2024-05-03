module Test.ProposalUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.ProposalUTxOs.CreateProposal qualified as CreateProposal
import Test.ProposalUTxOs.CloseProposal qualified as CloseProposal
import Test.ProposalUTxOs.UpdateProposal qualified as UpdateProposal

tests :: TestTree
tests = testGroup "Proposal UTxO Tests"
  [ CreateProposal.tests
  , CloseProposal.tests
  , UpdateProposal.tests
  ]
