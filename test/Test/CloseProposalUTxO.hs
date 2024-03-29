{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Test.CloseProposalUTxO
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import PlutusTx.Prelude
import Control.Monad (void)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Cardano.Node.Emulator.TimeSlot

import Test.Common
import CardanoOptions

-------------------------------------------------
-- Close Proposal UTxO Scenarios
-------------------------------------------------
successfullyCloseSingleProposalContract :: EmulatorTrace ()
successfullyCloseSingleProposalContract = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                                     $ mockWalletPaymentPubKeyHash
                                                     $ knownWallet 1) 
                                   Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-proposal-utxo(s)" h1 $
    CloseProposalParams
      { closeProposalBeaconsBurned = [[("Proposed",-1)]]
      , closeProposalBeaconRedeemer = BurnBeacons
      , closeProposalBeaconPolicies = [optionsBeaconPolicy1]
      , closeProposalOptionsVal = optionsValidator
      , closeProposalOptionsAddress = addr
      , closeProposalSpecificUTxOs = 
          [ ( proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      }

successfullyCloseMultipleSameProposalContracts :: EmulatorTrace ()
successfullyCloseMultipleSameProposalContracts = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                                     $ mockWalletPaymentPubKeyHash
                                                     $ knownWallet 1) 
                                   Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",2)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          , ( Just proposeDatum{premium = 5_000_000}
            , lovelaceValueOf 3_000_000
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-proposal-utxo(s)" h1 $
    CloseProposalParams
      { closeProposalBeaconsBurned = [[("Proposed",-2)]]
      , closeProposalBeaconRedeemer = BurnBeacons
      , closeProposalBeaconPolicies = [optionsBeaconPolicy1]
      , closeProposalOptionsVal = optionsValidator
      , closeProposalOptionsAddress = addr
      , closeProposalSpecificUTxOs = 
          [ ( proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          , ( proposeDatum{premium = 5_000_000}
            , lovelaceValueOf 3_000_000
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      }

successfullyCloseMultipleDifferentProposalContracts :: EmulatorTrace ()
successfullyCloseMultipleDifferentProposalContracts = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                                     $ mockWalletPaymentPubKeyHash
                                                     $ knownWallet 1) 
                                   Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      proposeDatum2 = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym4
        , currentAsset = testToken1
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken2
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                                     $ mockWalletPaymentPubKeyHash
                                                     $ knownWallet 1) 
                                   Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy4
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum2
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym4 "Proposed" 1
            )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 4

  callEndpoint @"close-proposal-utxo(s)" h1 $
    CloseProposalParams
      { closeProposalBeaconsBurned = [[("Proposed",-1)],[("Proposed",-1)]]
      , closeProposalBeaconRedeemer = BurnBeacons
      , closeProposalBeaconPolicies = [optionsBeaconPolicy1,optionsBeaconPolicy4]
      , closeProposalOptionsVal = optionsValidator
      , closeProposalOptionsAddress = addr
      , closeProposalSpecificUTxOs = 
          [ ( proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          , ( proposeDatum2
            , lovelaceValueOf 3_000_000
           <> singleton optionsBeaconPolicySym4 "Proposed" 1
            )
          ]
      }

datumIsNotProposedContract :: EmulatorTrace ()
datumIsNotProposedContract = do
  h1 <- activateContractWallet (knownWallet 3) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 3)
  
  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy1
      , assetsAddress = addr
      , assetsInfo = 
          [ ( Just assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-proposal-utxo(s)" h1 $
    CloseProposalParams
      { closeProposalBeaconsBurned = [[("Proposed",-5)]]
      , closeProposalBeaconRedeemer = BurnBeacons
      , closeProposalBeaconPolicies = [optionsBeaconPolicy1]
      , closeProposalOptionsVal = optionsValidator
      , closeProposalOptionsAddress = addr
      , closeProposalSpecificUTxOs = 
          [ ( assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      }

onlyProposedBeaconNotBurned :: EmulatorTrace ()
onlyProposedBeaconNotBurned = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                                     $ mockWalletPaymentPubKeyHash
                                                     $ knownWallet 1) 
                                   Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-proposal-utxo(s)" h1 $
    CloseProposalParams
      { closeProposalBeaconsBurned = []
      , closeProposalBeaconRedeemer = BurnBeacons
      , closeProposalBeaconPolicies = [optionsBeaconPolicy1]
      , closeProposalOptionsVal = optionsValidator
      , closeProposalOptionsAddress = addr
      , closeProposalSpecificUTxOs = 
          [ ( proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      }

atLeastOneProposedBeaconNotBurned :: EmulatorTrace ()
atLeastOneProposedBeaconNotBurned = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                                     $ mockWalletPaymentPubKeyHash
                                                     $ knownWallet 1) 
                                   Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",2)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          , ( Just proposeDatum{premium = 5_000_000}
            , lovelaceValueOf 3_000_000
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-proposal-utxo(s)" h1 $
    CloseProposalParams
      { closeProposalBeaconsBurned = [[("Proposed",-1)]]
      , closeProposalBeaconRedeemer = BurnBeacons
      , closeProposalBeaconPolicies = [optionsBeaconPolicy1]
      , closeProposalOptionsVal = optionsValidator
      , closeProposalOptionsAddress = addr
      , closeProposalSpecificUTxOs = 
          [ ( proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          , ( proposeDatum{premium = 5_000_000}
            , lovelaceValueOf 3_000_000
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      }

activeBeaconMinted :: EmulatorTrace ()
activeBeaconMinted = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                                     $ mockWalletPaymentPubKeyHash
                                                     $ knownWallet 1) 
                                   Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-proposal-utxo(s)" h1 $
    CloseProposalParams
      { closeProposalBeaconsBurned = [[("Proposed",-1),("Active",1)]]
      , closeProposalBeaconRedeemer = BurnBeacons
      , closeProposalBeaconPolicies = [optionsBeaconPolicy1]
      , closeProposalOptionsVal = optionsValidator
      , closeProposalOptionsAddress = addr
      , closeProposalSpecificUTxOs = 
          [ ( proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      }

stakingCredDidNotApprove :: EmulatorTrace ()
stakingCredDidNotApprove = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 4) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                                     $ mockWalletPaymentPubKeyHash
                                                     $ knownWallet 1) 
                                   Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-proposal-utxo(s)" h2 $
    CloseProposalParams
      { closeProposalBeaconsBurned = [[("Proposed",-1)]]
      , closeProposalBeaconRedeemer = BurnBeacons
      , closeProposalBeaconPolicies = [optionsBeaconPolicy1]
      , closeProposalOptionsVal = optionsValidator
      , closeProposalOptionsAddress = addr
      , closeProposalSpecificUTxOs = 
          [ ( proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      }

successfullyCloseInvalidProposal :: EmulatorTrace ()
successfullyCloseInvalidProposal = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                                     $ mockWalletPaymentPubKeyHash
                                                     $ knownWallet 1) 
                                   Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = []
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-proposal-utxo(s)" h1 $
    CloseProposalParams
      { closeProposalBeaconsBurned = []
      , closeProposalBeaconRedeemer = BurnBeacons
      , closeProposalBeaconPolicies = [optionsBeaconPolicy1]
      , closeProposalOptionsVal = optionsValidator
      , closeProposalOptionsAddress = addr
      , closeProposalSpecificUTxOs = 
          [ ( proposeDatum
            , lovelaceValueOf 3_000_000 
            )
          ]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
      lenientOpts = defaultCheckOptions & emulatorConfig .~ lenientConfig
  testGroup "Close Proposal UTxO(s)"
    [ checkPredicateOptions opts "Fail if datum is not ProposedContract"
        (Test.not assertNoFailedTransactions) datumIsNotProposedContract
    , checkPredicateOptions opts "Fail if only Proposed beacon not burned"
        (Test.not assertNoFailedTransactions) onlyProposedBeaconNotBurned
    , checkPredicateOptions opts "Fail if at least one Proposed beacon not burned"
        (Test.not assertNoFailedTransactions) atLeastOneProposedBeaconNotBurned
    , checkPredicateOptions opts "Fail if an Active beacon is minted in tx"
        (Test.not assertNoFailedTransactions) activeBeaconMinted
    , checkPredicateOptions opts "Fail if staking credential did not approve"
        (Test.not assertNoFailedTransactions) stakingCredDidNotApprove

    , checkPredicateOptions opts "Successfully close a single Proposal UTxO"
        assertNoFailedTransactions successfullyCloseSingleProposalContract
    , checkPredicateOptions opts "Successfully close multiple same Proposal UTxOs"
        assertNoFailedTransactions successfullyCloseMultipleSameProposalContracts
    , checkPredicateOptions lenientOpts "Successfully close multiple different Proposal UTxOs"
        assertNoFailedTransactions successfullyCloseMultipleDifferentProposalContracts
    , checkPredicateOptions opts "Successfully close invalid Proposal UTxO"
        assertNoFailedTransactions successfullyCloseInvalidProposal
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def lenientConfig successfullyCloseInvalidProposal