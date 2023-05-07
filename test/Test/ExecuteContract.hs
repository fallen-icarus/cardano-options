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

module Test.ExecuteContract
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
import Ledger.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash,alwaysSucceedPolicy)
import Ledger.TimeSlot
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)

import Test.Common
import CardanoOptions

-------------------------------------------------
-- Execution Scenarios
-------------------------------------------------
successfullyExecuteSingleContract :: EmulatorTrace ()
successfullyExecuteSingleContract = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred = PubKeyCredential
                         $ unPaymentPubKeyHash
                         $ mockWalletPaymentPubKeyHash
                         $ knownWallet 1
      assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        }
      optionsAddr = Address (ScriptCredential optionsValidatorHash)
                            (Just $ StakingHash optionsStakingCred)
  
  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = optionsAddr
      , assetsInfo = 
          [ ( Just assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 2

  let creatorAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        }
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy
      , proposeAddress = optionsAddr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySymbol "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 4

  targetHash <- txIdWithValue ( lovelaceValueOf 5_000_000 
                             <> singleton optionsBeaconPolicySymbol "Assets" 1
                             <> lovelaceValueOf 100_000_000
                              )

  let targetId = txIdAsToken targetHash
      activeDatum = ActiveContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId
        }
           
  callEndpoint @"accept-contract" h2 $
    AcceptParams
      { acceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1),(targetId,2)]
      , acceptBeaconRedeemer = MintActiveBeacon targetId optionsStakingCred
      , acceptBeaconPolicy = optionsBeaconPolicy
      , acceptOptionsVal = optionsValidator
      , acceptOptionsAddress = optionsAddr
      , acceptSpecificUTxOs = 
          [ ( proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySymbol "Proposed" 1
            )
          , ( assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , acceptChangeAddress = optionsAddr
      , acceptChangeOutput = 
          [ ( Just activeDatum
            , lovelaceValueOf 5_000_000
           <> singleton optionsBeaconPolicySymbol "Active" 1
           <> singleton optionsBeaconPolicySymbol targetId 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , acceptPremiumAddress = creatorAddr
      , acceptPremiumOutput = 
          [ ( Nothing
            ,  lovelaceValueOf 10_000_000
            <> lovelaceValueOf 3_000_000
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  callEndpoint @"execute-contract" h2 $
    ExecuteParams
      { executeBeaconsBurned = [("Active",-1),(targetId,-1)]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicy = optionsBeaconPolicy
      , executeVal = optionsValidator
      , executeAddress = optionsAddr
      , executeContractIds = [targetId]
      , executeSpecificUTxOs = 
          [ ( activeDatum
            , lovelaceValueOf 5_000_000
           <> singleton optionsBeaconPolicySymbol "Active" 1
           <> singleton optionsBeaconPolicySymbol targetId 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , executeCreatorAddress = creatorAddr
      , executeCreatorPayment =
          [ ( Nothing
            , lovelaceValueOf 5_000_000
           <> (uncurry singleton testToken1) 10
           <> singleton optionsBeaconPolicySymbol targetId 1
            )
          ]
      , executeWithTTE = True
      }

successfullyExecuteMultipleContracts :: EmulatorTrace ()
successfullyExecuteMultipleContracts = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred1 = PubKeyCredential
                          $ unPaymentPubKeyHash
                          $ mockWalletPaymentPubKeyHash
                          $ knownWallet 1
      assetsDatum1 = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        }
      assetsDatum2 = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 50_000_000
        }
      optionsAddr1 = Address (ScriptCredential optionsValidatorHash)
                             (Just $ StakingHash optionsStakingCred1)
      creatorAddr1 = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  
  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = optionsAddr1
      , assetsInfo = 
          [ ( Just assetsDatum1
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
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
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = optionsAddr1
      , assetsInfo = 
          [ ( Just assetsDatum2
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 50_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 4

  let proposeDatum1 = ProposedContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        }
      proposeDatum2 = ProposedContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 50_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        }
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy
      , proposeAddress = optionsAddr1
      , proposeInfo = 
          [ ( Just proposeDatum1
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySymbol "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 6

  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy
      , proposeAddress = optionsAddr1
      , proposeInfo = 
          [ ( Just proposeDatum2
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySymbol "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 8

  targetHash1 <- txIdWithValue ( lovelaceValueOf 5_000_000 
                              <> singleton optionsBeaconPolicySymbol "Assets" 1
                              <> lovelaceValueOf 100_000_000
                               )
  targetHash2 <- txIdWithValue ( lovelaceValueOf 5_000_000 
                              <> singleton optionsBeaconPolicySymbol "Assets" 1
                              <> lovelaceValueOf 50_000_000
                               )


  let targetId1 = txIdAsToken targetHash1
      targetId2 = txIdAsToken targetHash2
      activeDatum1 = ActiveContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId1
        }
      activeDatum2 = ActiveContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 50_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr1
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId2
        }

  callEndpoint @"accept-contract" h2 $
    AcceptParams
      { acceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1),(targetId1,2)]
      , acceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , acceptBeaconPolicy = optionsBeaconPolicy
      , acceptOptionsVal = optionsValidator
      , acceptOptionsAddress = optionsAddr1
      , acceptSpecificUTxOs = 
          [ ( proposeDatum1
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySymbol "Proposed" 1
            )
          , ( assetsDatum1
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , acceptChangeAddress = optionsAddr1
      , acceptChangeOutput = 
          [ ( Just activeDatum1
            , lovelaceValueOf 5_000_000
           <> singleton optionsBeaconPolicySymbol "Active" 1
           <> singleton optionsBeaconPolicySymbol targetId1 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , acceptPremiumAddress = creatorAddr1
      , acceptPremiumOutput = 
          [ ( Nothing
            ,  lovelaceValueOf 10_000_000
            <> lovelaceValueOf 3_000_000
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 10

  callEndpoint @"accept-contract" h2 $
    AcceptParams
      { acceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1),(targetId2,2)]
      , acceptBeaconRedeemer = MintActiveBeacon targetId2 optionsStakingCred1
      , acceptBeaconPolicy = optionsBeaconPolicy
      , acceptOptionsVal = optionsValidator
      , acceptOptionsAddress = optionsAddr1
      , acceptSpecificUTxOs = 
          [ ( proposeDatum2
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySymbol "Proposed" 1
            )
          , ( assetsDatum2
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 50_000_000
            )
          ]
      , acceptChangeAddress = optionsAddr1
      , acceptChangeOutput = 
          [ ( Just activeDatum2
            , lovelaceValueOf 5_000_000
           <> singleton optionsBeaconPolicySymbol "Active" 1
           <> singleton optionsBeaconPolicySymbol targetId2 1
           <> lovelaceValueOf 50_000_000
            )
          ]
      , acceptPremiumAddress = creatorAddr1
      , acceptPremiumOutput = 
          [ ( Nothing
            ,  lovelaceValueOf 10_000_000
            <> lovelaceValueOf 3_000_000
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 12

  callEndpoint @"execute-contract" h2 $
    ExecuteParams
      { executeBeaconsBurned = [("Active",-2),(targetId1,-1),(targetId2,-1)]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicy = optionsBeaconPolicy
      , executeVal = optionsValidator
      , executeAddress = optionsAddr1
      , executeContractIds = [targetId]
      , executeSpecificUTxOs = 
          [ ( activeDatum
            , lovelaceValueOf 5_000_000
           <> singleton optionsBeaconPolicySymbol "Active" 1
           <> singleton optionsBeaconPolicySymbol targetId 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      , executeCreatorAddress = creatorAddr
      , executeCreatorPayment =
          [ ( Nothing
            , lovelaceValueOf 5_000_000
           <> (uncurry singleton testToken1) 10
           <> singleton optionsBeaconPolicySymbol targetId 1
            )
          ]
      , executeWithTTE = True
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Execute Contract(s)"
    [

    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig successfullyExecuteSingleContract