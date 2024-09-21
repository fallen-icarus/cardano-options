module Test.ProposalUTxOs.CreateProposal where

import Test.Tasty (TestTree,testGroup)

import Test.ProposalUTxOs.CreateProposal.Regressions qualified as Regressions
import Test.ProposalUTxOs.CreateProposal.Failures qualified as Failures
import Test.ProposalUTxOs.CreateProposal.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Creating Proposal UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
