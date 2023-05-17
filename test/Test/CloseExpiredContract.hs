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

module Test.CloseExpiredContract
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import Control.Monad (void)
import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash)
import Cardano.Node.Emulator.TimeSlot

import Test.Common
import CardanoOptions

-------------------------------------------------
-- Close Scenarios
-------------------------------------------------
successfullyCloseSingleContract :: EmulatorTrace ()
successfullyCloseSingleContract = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred1 = PubKeyCredential
                          $ unPaymentPubKeyHash
                          $ mockWalletPaymentPubKeyHash
                          $ knownWallet 1
      assetsDatum1 = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        }
      optionsAddr1 = Address (ScriptCredential optionsValidatorHash)
                             (Just $ StakingHash optionsStakingCred1)
      creatorAddr1 = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  
  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy1
      , assetsAddress = optionsAddr1
      , assetsInfo = 
          [ ( Just assetsDatum1
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 2

  let proposeDatum1 = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        }
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = optionsAddr1
      , proposeInfo = 
          [ ( Just proposeDatum1
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 6

  targetHash1 <- txIdWithValue ( lovelaceValueOf 5_000_000 
                              <> singleton optionsBeaconPolicySym1 "Assets" 1
                              <> lovelaceValueOf 100_000_000
                               )

  let targetId1 = txIdAsToken targetHash1
      activeDatum1 = ActiveContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId1
        }
           
  callEndpoint @"accept-contract(s)" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [[("Active",1),("Assets",-1),("Proposed",-1),(targetId1,2)]]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicies = [optionsBeaconPolicy1]
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySym1 "Proposed" 1
              )
            , ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySym1 "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptPremiumAddresses = [creatorAddr1]
      , multiAcceptPremiumOutput = 
          [ [ ( Nothing
              ,  lovelaceValueOf 10_000_000
              <> lovelaceValueOf 3_000_000
              )
            ]
          ]
      , multiAcceptDatumAsInline = True
      , multiAcceptWithTTL = True
      }

  void $ waitUntilSlot 21

  callEndpoint @"close-expired-contract(s)" h1 $
    ClaimParams
      { claimBeaconsBurned = [("Active",-1),(targetId1,-1)]
      , claimBeaconRedeemer = BurnBeacons
      , claimBeaconPolicy = optionsBeaconPolicy1
      , claimVal = optionsValidator
      , claimAddress = optionsAddr1
      , claimSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
          <> singleton optionsBeaconPolicySym1 "Active" 1
          <> singleton optionsBeaconPolicySym1 targetId1 1
          <> lovelaceValueOf 100_000_000
            )
          ]
      , claimWithTTL = True
      }

successfullyCloseMultipleContracts :: EmulatorTrace ()
successfullyCloseMultipleContracts = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred1 = PubKeyCredential
                          $ unPaymentPubKeyHash
                          $ mockWalletPaymentPubKeyHash
                          $ knownWallet 1
      optionsStakingCred2 = PubKeyCredential
                          $ unPaymentPubKeyHash
                          $ mockWalletPaymentPubKeyHash
                          $ knownWallet 5
      assetsDatum1 = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        }
      assetsDatum2 = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 50_000_000
        , desiredAsset = testToken1
        }
      optionsAddr1 = Address (ScriptCredential optionsValidatorHash)
                             (Just $ StakingHash optionsStakingCred1)
      optionsAddr2 = Address (ScriptCredential optionsValidatorHash)
                             (Just $ StakingHash optionsStakingCred2)
      creatorAddr1 = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      creatorAddr2 = 
        Address ( PubKeyCredential 
                $ unPaymentPubKeyHash 
                $ mockWalletPaymentPubKeyHash 
                $ knownWallet 1
                ) 
                Nothing
  
  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy1
      , assetsAddress = optionsAddr1
      , assetsInfo = 
          [ ( Just assetsDatum1
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy1
      , assetsAddress = optionsAddr1
      , assetsInfo = 
          [ ( Just assetsDatum2
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Assets" 1
           <> lovelaceValueOf 50_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 4

  let proposeDatum1 = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        }
      proposeDatum2 = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 50_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr2
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        }
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = optionsAddr1
      , proposeInfo = 
          [ ( Just proposeDatum1
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 6

  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = optionsAddr1
      , proposeInfo = 
          [ ( Just proposeDatum2
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 8

  targetHash1 <- txIdWithValue ( lovelaceValueOf 5_000_000 
                              <> singleton optionsBeaconPolicySym1 "Assets" 1
                              <> lovelaceValueOf 100_000_000
                               )
  targetHash2 <- txIdWithValue ( lovelaceValueOf 5_000_000 
                              <> singleton optionsBeaconPolicySym1 "Assets" 1
                              <> lovelaceValueOf 50_000_000
                               )


  let targetId1 = txIdAsToken targetHash1
      targetId2 = txIdAsToken targetHash2
      activeDatum1 = ActiveContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId1
        }
      activeDatum2 = ActiveContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 50_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr2
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId2
        }
           
  callEndpoint @"accept-contract(s)" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [[("Active",1),("Assets",-1),("Proposed",-1),(targetId1,2)]]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicies = [optionsBeaconPolicy1]
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySym1 "Proposed" 1
              )
            , ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySym1 "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptPremiumAddresses = [creatorAddr1]
      , multiAcceptPremiumOutput = 
          [ [ ( Nothing
              ,  lovelaceValueOf 10_000_000
              <> lovelaceValueOf 3_000_000
              )
            ]
          ]
      , multiAcceptDatumAsInline = True
      , multiAcceptWithTTL = True
      }

  void $ waitUntilSlot 10

  callEndpoint @"accept-contract(s)" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [[("Active",1),("Assets",-1),("Proposed",-1),(targetId2,2)]]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId2 optionsStakingCred1
      , multiAcceptBeaconPolicies = [optionsBeaconPolicy1]
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum2
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySym1 "Proposed" 1
              )
            , ( assetsDatum2
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySym1 "Assets" 1
             <> lovelaceValueOf 50_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum2
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId2 1
            <> lovelaceValueOf 50_000_000
              )
            ]
          ]
      , multiAcceptPremiumAddresses = [creatorAddr2]
      , multiAcceptPremiumOutput = 
          [ [ ( Nothing
              ,  lovelaceValueOf 10_000_000
              <> lovelaceValueOf 3_000_000
              )
            ]
          ]
      , multiAcceptDatumAsInline = True
      , multiAcceptWithTTL = True
      }

  void $ waitUntilSlot 21

  callEndpoint @"close-expired-contract(s)" h1 $
    ClaimParams
      { claimBeaconsBurned = [("Active",-2),(targetId1,-1),(targetId2,-1)]
      , claimBeaconRedeemer = BurnBeacons
      , claimBeaconPolicy = optionsBeaconPolicy1
      , claimVal = optionsValidator
      , claimAddress = optionsAddr1
      , claimSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
          <> singleton optionsBeaconPolicySym1 "Active" 1
          <> singleton optionsBeaconPolicySym1 targetId1 1
          <> lovelaceValueOf 100_000_000
            )
          , ( activeDatum2
            , lovelaceValueOf 5_000_000
          <> singleton optionsBeaconPolicySym1 "Active" 1
          <> singleton optionsBeaconPolicySym1 targetId2 1
          <> lovelaceValueOf 50_000_000
            )
          ]
      , claimWithTTL = True
      }

datumIsNotAnActiveDatum :: EmulatorTrace ()
datumIsNotAnActiveDatum = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
                           $ knownWallet 1)
  
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

  callEndpoint @"close-expired-contract(s)" h1 $
    ClaimParams
      { claimBeaconsBurned = []
      , claimBeaconRedeemer = BurnBeacons
      , claimBeaconPolicy = optionsBeaconPolicy1
      , claimVal = optionsValidator
      , claimAddress = addr
      , claimSpecificUTxOs =
          [ ( assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Assets" 1
           <> lovelaceValueOf 100_000_000
           )
          ]
      , claimWithTTL = True
      }

stakingCredDidNotApprove :: EmulatorTrace ()
stakingCredDidNotApprove = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred1 = PubKeyCredential
                          $ unPaymentPubKeyHash
                          $ mockWalletPaymentPubKeyHash
                          $ knownWallet 1
      assetsDatum1 = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        }
      optionsAddr1 = Address (ScriptCredential optionsValidatorHash)
                             (Just $ StakingHash optionsStakingCred1)
      creatorAddr1 = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  
  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy1
      , assetsAddress = optionsAddr1
      , assetsInfo = 
          [ ( Just assetsDatum1
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 2

  let proposeDatum1 = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        }
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = optionsAddr1
      , proposeInfo = 
          [ ( Just proposeDatum1
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 6

  targetHash1 <- txIdWithValue ( lovelaceValueOf 5_000_000 
                              <> singleton optionsBeaconPolicySym1 "Assets" 1
                              <> lovelaceValueOf 100_000_000
                               )

  let targetId1 = txIdAsToken targetHash1
      activeDatum1 = ActiveContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId1
        }
           
  callEndpoint @"accept-contract(s)" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [[("Active",1),("Assets",-1),("Proposed",-1),(targetId1,2)]]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicies = [optionsBeaconPolicy1]
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySym1 "Proposed" 1
              )
            , ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySym1 "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptPremiumAddresses = [creatorAddr1]
      , multiAcceptPremiumOutput = 
          [ [ ( Nothing
              ,  lovelaceValueOf 10_000_000
              <> lovelaceValueOf 3_000_000
              )
            ]
          ]
      , multiAcceptDatumAsInline = True
      , multiAcceptWithTTL = True
      }

  void $ waitUntilSlot 21

  callEndpoint @"close-expired-contract(s)" h2 $
    ClaimParams
      { claimBeaconsBurned = [("Active",-1),(targetId1,-1)]
      , claimBeaconRedeemer = BurnBeacons
      , claimBeaconPolicy = optionsBeaconPolicy1
      , claimVal = optionsValidator
      , claimAddress = optionsAddr1
      , claimSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
          <> singleton optionsBeaconPolicySym1 "Active" 1
          <> singleton optionsBeaconPolicySym1 targetId1 1
          <> lovelaceValueOf 100_000_000
            )
          ]
      , claimWithTTL = True
      }

successfullySpendInvalidActiveUTxO :: EmulatorTrace ()
successfullySpendInvalidActiveUTxO = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred1 = PubKeyCredential
                          $ unPaymentPubKeyHash
                          $ mockWalletPaymentPubKeyHash
                          $ knownWallet 1
      assetsDatum1 = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        }
      optionsAddr1 = Address (ScriptCredential optionsValidatorHash)
                             (Just $ StakingHash optionsStakingCred1)
      creatorAddr1 = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  
  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy1
      , assetsAddress = optionsAddr1
      , assetsInfo = 
          [ ( Just assetsDatum1
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 2

  let proposeDatum1 = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        }
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = optionsAddr1
      , proposeInfo = 
          [ ( Just proposeDatum1
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 6

  targetHash1 <- txIdWithValue ( lovelaceValueOf 5_000_000 
                              <> singleton optionsBeaconPolicySym1 "Assets" 1
                              <> lovelaceValueOf 100_000_000
                               )

  let targetId1 = txIdAsToken targetHash1
      activeDatum1 = ActiveContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId1
        }
           
  callEndpoint @"accept-contract(s)" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = []
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicies = [optionsBeaconPolicy1]
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          []
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptPremiumAddresses = [creatorAddr1]
      , multiAcceptPremiumOutput = 
          []
      , multiAcceptDatumAsInline = True
      , multiAcceptWithTTL = True
      }

  void $ waitUntilSlot 21

  callEndpoint @"close-expired-contract(s)" h1 $
    ClaimParams
      { claimBeaconsBurned = []
      , claimBeaconRedeemer = BurnBeacons
      , claimBeaconPolicy = optionsBeaconPolicy1
      , claimVal = optionsValidator
      , claimAddress = optionsAddr1
      , claimSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
          <> lovelaceValueOf 100_000_000
            )
          ]
      , claimWithTTL = True
      }

contractIsNotExpired :: EmulatorTrace ()
contractIsNotExpired = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred1 = PubKeyCredential
                          $ unPaymentPubKeyHash
                          $ mockWalletPaymentPubKeyHash
                          $ knownWallet 1
      assetsDatum1 = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        }
      optionsAddr1 = Address (ScriptCredential optionsValidatorHash)
                             (Just $ StakingHash optionsStakingCred1)
      creatorAddr1 = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  
  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy1
      , assetsAddress = optionsAddr1
      , assetsInfo = 
          [ ( Just assetsDatum1
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 2

  let proposeDatum1 = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        }
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = optionsAddr1
      , proposeInfo = 
          [ ( Just proposeDatum1
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 6

  targetHash1 <- txIdWithValue ( lovelaceValueOf 5_000_000 
                              <> singleton optionsBeaconPolicySym1 "Assets" 1
                              <> lovelaceValueOf 100_000_000
                               )

  let targetId1 = txIdAsToken targetHash1
      activeDatum1 = ActiveContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId1
        }
           
  callEndpoint @"accept-contract(s)" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [[("Active",1),("Assets",-1),("Proposed",-1),(targetId1,2)]]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicies = [optionsBeaconPolicy1]
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySym1 "Proposed" 1
              )
            , ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySym1 "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptPremiumAddresses = [creatorAddr1]
      , multiAcceptPremiumOutput = 
          [ [ ( Nothing
              ,  lovelaceValueOf 10_000_000
              <> lovelaceValueOf 3_000_000
              )
            ]
          ]
      , multiAcceptDatumAsInline = True
      , multiAcceptWithTTL = True
      }

  void $ waitUntilSlot 8

  callEndpoint @"close-expired-contract(s)" h1 $
    ClaimParams
      { claimBeaconsBurned = [("Active",-1),(targetId1,-1)]
      , claimBeaconRedeemer = BurnBeacons
      , claimBeaconPolicy = optionsBeaconPolicy1
      , claimVal = optionsValidator
      , claimAddress = optionsAddr1
      , claimSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
          <> singleton optionsBeaconPolicySym1 "Active" 1
          <> singleton optionsBeaconPolicySym1 targetId1 1
          <> lovelaceValueOf 100_000_000
            )
          ]
      , claimWithTTL = True
      }

contractIdNotBurned :: EmulatorTrace ()
contractIdNotBurned = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred1 = PubKeyCredential
                          $ unPaymentPubKeyHash
                          $ mockWalletPaymentPubKeyHash
                          $ knownWallet 1
      assetsDatum1 = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        }
      optionsAddr1 = Address (ScriptCredential optionsValidatorHash)
                             (Just $ StakingHash optionsStakingCred1)
      creatorAddr1 = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  
  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy1
      , assetsAddress = optionsAddr1
      , assetsInfo = 
          [ ( Just assetsDatum1
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 2

  let proposeDatum1 = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        }
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = optionsAddr1
      , proposeInfo = 
          [ ( Just proposeDatum1
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 6

  targetHash1 <- txIdWithValue ( lovelaceValueOf 5_000_000 
                              <> singleton optionsBeaconPolicySym1 "Assets" 1
                              <> lovelaceValueOf 100_000_000
                               )

  let targetId1 = txIdAsToken targetHash1
      activeDatum1 = ActiveContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId1
        }
           
  callEndpoint @"accept-contract(s)" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [[("Active",1),("Assets",-1),("Proposed",-1),(targetId1,2)]]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicies = [optionsBeaconPolicy1]
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySym1 "Proposed" 1
              )
            , ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySym1 "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptPremiumAddresses = [creatorAddr1]
      , multiAcceptPremiumOutput = 
          [ [ ( Nothing
              ,  lovelaceValueOf 10_000_000
              <> lovelaceValueOf 3_000_000
              )
            ]
          ]
      , multiAcceptDatumAsInline = True
      , multiAcceptWithTTL = True
      }

  void $ waitUntilSlot 21

  callEndpoint @"close-expired-contract(s)" h1 $
    ClaimParams
      { claimBeaconsBurned = [("Active",-1)]
      , claimBeaconRedeemer = BurnBeacons
      , claimBeaconPolicy = optionsBeaconPolicy1
      , claimVal = optionsValidator
      , claimAddress = optionsAddr1
      , claimSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
          <> singleton optionsBeaconPolicySym1 "Active" 1
          <> singleton optionsBeaconPolicySym1 targetId1 1
          <> lovelaceValueOf 100_000_000
            )
          ]
      , claimWithTTL = True
      }

activeBeaconNotBurned :: EmulatorTrace ()
activeBeaconNotBurned = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred1 = PubKeyCredential
                          $ unPaymentPubKeyHash
                          $ mockWalletPaymentPubKeyHash
                          $ knownWallet 1
      assetsDatum1 = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        }
      optionsAddr1 = Address (ScriptCredential optionsValidatorHash)
                             (Just $ StakingHash optionsStakingCred1)
      creatorAddr1 = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  
  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy1
      , assetsAddress = optionsAddr1
      , assetsInfo = 
          [ ( Just assetsDatum1
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 2

  let proposeDatum1 = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        }
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = optionsAddr1
      , proposeInfo = 
          [ ( Just proposeDatum1
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 6

  targetHash1 <- txIdWithValue ( lovelaceValueOf 5_000_000 
                              <> singleton optionsBeaconPolicySym1 "Assets" 1
                              <> lovelaceValueOf 100_000_000
                               )

  let targetId1 = txIdAsToken targetHash1
      activeDatum1 = ActiveContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId1
        }
           
  callEndpoint @"accept-contract(s)" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [[("Active",1),("Assets",-1),("Proposed",-1),(targetId1,2)]]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicies = [optionsBeaconPolicy1]
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySym1 "Proposed" 1
              )
            , ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySym1 "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptPremiumAddresses = [creatorAddr1]
      , multiAcceptPremiumOutput = 
          [ [ ( Nothing
              ,  lovelaceValueOf 10_000_000
              <> lovelaceValueOf 3_000_000
              )
            ]
          ]
      , multiAcceptDatumAsInline = True
      , multiAcceptWithTTL = True
      }

  void $ waitUntilSlot 21

  callEndpoint @"close-expired-contract(s)" h1 $
    ClaimParams
      { claimBeaconsBurned = [(targetId1,-1)]
      , claimBeaconRedeemer = BurnBeacons
      , claimBeaconPolicy = optionsBeaconPolicy1
      , claimVal = optionsValidator
      , claimAddress = optionsAddr1
      , claimSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
          <> singleton optionsBeaconPolicySym1 "Active" 1
          <> singleton optionsBeaconPolicySym1 targetId1 1
          <> lovelaceValueOf 100_000_000
            )
          ]
      , claimWithTTL = True
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Close Expired Contract(s)"
    [ checkPredicateOptions opts "Fail if datum is not an ActiveContract datum"
        (Test.not assertNoFailedTransactions) datumIsNotAnActiveDatum
    , checkPredicateOptions opts "Fail if staking credential did not approve"
        (Test.not assertNoFailedTransactions) stakingCredDidNotApprove
    , checkPredicateOptions opts "Fail if contract is still active"
        (Test.not assertNoFailedTransactions) contractIsNotExpired
    , checkPredicateOptions opts "Fail if ContractID not burned"
        (Test.not assertNoFailedTransactions) contractIdNotBurned
    , checkPredicateOptions opts "Fail if Active beacon not burned"
        (Test.not assertNoFailedTransactions) activeBeaconNotBurned

    , checkPredicateOptions opts "Successfully close single contract"
        assertNoFailedTransactions successfullyCloseSingleContract
    , checkPredicateOptions opts "Successfully close multiple contracts"
        assertNoFailedTransactions successfullyCloseMultipleContracts
    , checkPredicateOptions opts "Successfully close invalid Active UTxO"
        assertNoFailedTransactions successfullySpendInvalidActiveUTxO
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig contractIsNotExpired