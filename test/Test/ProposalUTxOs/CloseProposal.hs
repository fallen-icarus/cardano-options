module Test.ProposalUTxOs.CloseProposal where

import Test.Tasty (TestTree,testGroup)

import Test.ProposalUTxOs.CloseProposal.Regressions qualified as Regressions
import Test.ProposalUTxOs.CloseProposal.Failures qualified as Failures
import Test.ProposalUTxOs.CloseProposal.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Closing Proposal UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
