module Test.ProposalUTxOs.UpdateProposal where

import Test.Tasty (TestTree,testGroup)

import Test.ProposalUTxOs.UpdateProposal.Regressions qualified as Regressions
import Test.ProposalUTxOs.UpdateProposal.Failures qualified as Failures
import Test.ProposalUTxOs.UpdateProposal.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Updating Proposal UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
