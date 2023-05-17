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
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash)
import Cardano.Node.Emulator.TimeSlot

import Test.Common
import CardanoOptions

-------------------------------------------------
-- Execution Scenarios
-------------------------------------------------
successfullyExecuteSingleContract :: EmulatorTrace ()
successfullyExecuteSingleContract = do
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

  callEndpoint @"execute-contract(s)" h2 $
    ExecuteParams
      { executeBeaconsBurned = [[("Active",-1),(targetId1,-1)]]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr1]
      , executeCreatorPayments =
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 10
            <> singleton optionsBeaconPolicySym1 targetId1 1
              )
            ]
          ]
      , executeWithTTE = True
      }

successfullyExecuteMultipleContractsFromSameAddress :: EmulatorTrace ()
successfullyExecuteMultipleContractsFromSameAddress = do
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

  void $ waitUntilSlot 12

  callEndpoint @"execute-contract(s)" h2 $
    ExecuteParams
      { executeBeaconsBurned = 
          [ [("Active",-2),(targetId1,-1),(targetId2,-1)]
          ]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1,optionsAddr1]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          , [ ( activeDatum2
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId2 1
            <> lovelaceValueOf 50_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr1,creatorAddr2]
      , executeCreatorPayments =
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 10
            <> singleton optionsBeaconPolicySym1 targetId1 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 5
            <> singleton optionsBeaconPolicySym1 targetId2 1
              )
            ]
          ]
      , executeWithTTE = True
      }

successfullyExecuteMultipleContractsFromDifferentAddressesButSamePolicy :: EmulatorTrace ()
successfullyExecuteMultipleContractsFromDifferentAddressesButSamePolicy = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints
  h3 <- activateContractWallet (knownWallet 6) endpoints

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

  callEndpoint @"create-assets-utxo" h2 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy1
      , assetsAddress = optionsAddr2
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

  callEndpoint @"propose-contract(s)" h2 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = optionsAddr2
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
           
  callEndpoint @"accept-contract(s)" h3 $
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

  callEndpoint @"accept-contract(s)" h3 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [[("Active",1),("Assets",-1),("Proposed",-1),(targetId2,2)]]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId2 optionsStakingCred2
      , multiAcceptBeaconPolicies = [optionsBeaconPolicy1]
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr2]
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
      , multiAcceptChangeAddresses = [optionsAddr2]
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

  void $ waitUntilSlot 12

  callEndpoint @"execute-contract(s)" h3 $
    ExecuteParams
      { executeBeaconsBurned = 
          [ [("Active",-2),(targetId1,-1),(targetId2,-1)]
          ]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1,optionsAddr2]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          , [ ( activeDatum2
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId2 1
            <> lovelaceValueOf 50_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr1,creatorAddr2]
      , executeCreatorPayments =
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 10
            <> singleton optionsBeaconPolicySym1 targetId1 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 5
            <> singleton optionsBeaconPolicySym1 targetId2 1
              )
            ]
          ]
      , executeWithTTE = True
      }

successfullyExecuteMultipleContractsFromDifferentAddressesAndPolicy :: EmulatorTrace ()
successfullyExecuteMultipleContractsFromDifferentAddressesAndPolicy = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints
  h3 <- activateContractWallet (knownWallet 6) endpoints

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
        { beaconSymbol = optionsBeaconPolicySym2
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 50_000_000
        , desiredAsset = testToken2
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

  callEndpoint @"create-assets-utxo" h2 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy2
      , assetsAddress = optionsAddr2
      , assetsInfo = 
          [ ( Just assetsDatum2
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym2 "Assets" 1
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
        { beaconSymbol = optionsBeaconPolicySym2
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 50_000_000
        , desiredAsset = testToken2
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

  callEndpoint @"propose-contract(s)" h2 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy2
      , proposeAddress = optionsAddr2
      , proposeInfo = 
          [ ( Just proposeDatum2
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym2 "Proposed" 1
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
                              <> singleton optionsBeaconPolicySym2 "Assets" 1
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
        { beaconSymbol = optionsBeaconPolicySym2
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 50_000_000
        , desiredAsset = testToken2
        , strikePrice = unsafeRatio 1 10_000_000
        , creatorAddress = creatorAddr2
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId2
        }
           
  callEndpoint @"accept-contract(s)" h3 $
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

  callEndpoint @"accept-contract(s)" h3 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [[("Active",1),("Assets",-1),("Proposed",-1),(targetId2,2)]]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId2 optionsStakingCred2
      , multiAcceptBeaconPolicies = [optionsBeaconPolicy2]
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr2]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum2
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySym2 "Proposed" 1
              )
            , ( assetsDatum2
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySym2 "Assets" 1
             <> lovelaceValueOf 50_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr2]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum2
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym2 "Active" 1
            <> singleton optionsBeaconPolicySym2 targetId2 1
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

  void $ waitUntilSlot 12

  callEndpoint @"execute-contract(s)" h3 $
    ExecuteParams
      { executeBeaconsBurned = 
          [ [("Active",-1),(targetId1,-1)]
          , [("Active",-1),(targetId2,-1)]
          ]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1,optionsBeaconPolicy2]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1,optionsAddr2]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          , [ ( activeDatum2
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym2 "Active" 1
            <> singleton optionsBeaconPolicySym2 targetId2 1
            <> lovelaceValueOf 50_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr1,creatorAddr2]
      , executeCreatorPayments =
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 10
            <> singleton optionsBeaconPolicySym1 targetId1 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken2) 5
            <> singleton optionsBeaconPolicySym2 targetId2 1
              )
            ]
          ]
      , executeWithTTE = True
      }

missingActiveBeacon :: EmulatorTrace ()
missingActiveBeacon = do
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

  void $ waitUntilSlot 8

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
              )
            ]
          ]
      , multiAcceptPremiumAddresses = [creatorAddr1]
      , multiAcceptPremiumOutput = 
          []
      , multiAcceptDatumAsInline = True
      , multiAcceptWithTTL = True
      }

  void $ waitUntilSlot 10

  callEndpoint @"execute-contract(s)" h2 $
    ExecuteParams
      { executeBeaconsBurned = []
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr1]
      , executeCreatorPayments =
          []
      , executeWithTTE = True
      }

contractIsExpired :: EmulatorTrace ()
contractIsExpired = do
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

  void $ waitUntilSlot 22

  callEndpoint @"execute-contract(s)" h2 $
    ExecuteParams
      { executeBeaconsBurned = [[("Active",-1),(targetId1,-1)]]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr1]
      , executeCreatorPayments =
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 10
            <> singleton optionsBeaconPolicySym1 targetId1 1
              )
            ]
          ]
      , executeWithTTE = True
      }

bothContractIdsBurned :: EmulatorTrace ()
bothContractIdsBurned = do
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

  callEndpoint @"execute-contract(s)" h2 $
    ExecuteParams
      { executeBeaconsBurned = [[("Active",-1),(targetId1,-2)]]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr1]
      , executeCreatorPayments =
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 10
              )
            ]
          ]
      , executeWithTTE = True
      }

noContractIDsBurned :: EmulatorTrace ()
noContractIDsBurned = do
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

  callEndpoint @"execute-contract(s)" h2 $
    ExecuteParams
      { executeBeaconsBurned = [[("Active",-1)]]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr1]
      , executeCreatorPayments =
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 10
            <> singleton optionsBeaconPolicySym1 targetId1 1
              )
            ]
          ]
      , executeWithTTE = True
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

  void $ waitUntilSlot 8

  callEndpoint @"execute-contract(s)" h2 $
    ExecuteParams
      { executeBeaconsBurned = [[(targetId1,-1)]]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr1]
      , executeCreatorPayments =
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 10
            <> singleton optionsBeaconPolicySym1 targetId1 1
              )
            ]
          ]
      , executeWithTTE = True
      }

creatorNotPaidReceipt :: EmulatorTrace ()
creatorNotPaidReceipt = do
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

  callEndpoint @"execute-contract(s)" h2 $
    ExecuteParams
      { executeBeaconsBurned = [[("Active",-1),(targetId1,-1)]]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr1]
      , executeCreatorPayments =
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 10
              )
            ]
          ]
      , executeWithTTE = True
      }

creatorNotPaidEnoughDesiredAsset :: EmulatorTrace ()
creatorNotPaidEnoughDesiredAsset = do
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

  callEndpoint @"execute-contract(s)" h2 $
    ExecuteParams
      { executeBeaconsBurned = [[("Active",-1),(targetId1,-1)]]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr1]
      , executeCreatorPayments =
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 9
            <> singleton optionsBeaconPolicySym1 targetId1 1
              )
            ]
          ]
      , executeWithTTE = True
      }

creatorNotReturnedMinDeposit :: EmulatorTrace ()
creatorNotReturnedMinDeposit = do
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

  callEndpoint @"execute-contract(s)" h2 $
    ExecuteParams
      { executeBeaconsBurned = [[("Active",-1),(targetId1,-1)]]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr1]
      , executeCreatorPayments =
          [ [ ( Nothing
              , lovelaceValueOf 3_000_000
            <> (uncurry singleton testToken1) 10
            <> singleton optionsBeaconPolicySym1 targetId1 1
              )
            ]
          ]
      , executeWithTTE = True
      }

paymentToWrongAddress :: EmulatorTrace ()
paymentToWrongAddress = do
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

  callEndpoint @"execute-contract(s)" h2 $
    ExecuteParams
      { executeBeaconsBurned = [[("Active",-1),(targetId1,-1)]]
      , executeBeaconRedeemer = BurnBeacons
      , executeBeaconPolicies = [optionsBeaconPolicy1]
      , executeVal = optionsValidator
      , executeAddresses = [optionsAddr1]
      , executeSpecificUTxOs = 
          [ [ ( activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , executeCreatorAddresses = [creatorAddr2]
      , executeCreatorPayments =
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
            <> (uncurry singleton testToken1) 10
            <> singleton optionsBeaconPolicySym1 targetId1 1
              )
            ]
          ]
      , executeWithTTE = True
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
      lenientOpts = defaultCheckOptions & emulatorConfig .~ lenientConfig
  testGroup "Execute Contract(s)"
    [ checkPredicateOptions opts "Fail if input is missing Active beacon"
        (Test.not assertNoFailedTransactions) missingActiveBeacon
    , checkPredicateOptions opts "Fail if contract is expired"
        (Test.not assertNoFailedTransactions) contractIsExpired
    , checkPredicateOptions opts "Fail if both ContractIDs burned"
        (Test.not assertNoFailedTransactions) bothContractIdsBurned
    , checkPredicateOptions opts "Fail if no ContractIDs burned"
        (Test.not assertNoFailedTransactions) noContractIDsBurned
    , checkPredicateOptions opts "Fail if Active beacons not burned"
        (Test.not assertNoFailedTransactions) activeBeaconNotBurned
    , checkPredicateOptions opts "Fail if other ContractID not sent to with payment"
        (Test.not assertNoFailedTransactions) creatorNotPaidReceipt
    , checkPredicateOptions opts "Fail if not enough desiredAsset sent to creator"
        (Test.not assertNoFailedTransactions) creatorNotPaidEnoughDesiredAsset
    , checkPredicateOptions opts "Fail if min deposit not returned to creator"
        (Test.not assertNoFailedTransactions) creatorNotReturnedMinDeposit
    , checkPredicateOptions opts "Fail if payment made to wrong address"
        (Test.not assertNoFailedTransactions) paymentToWrongAddress

    , checkPredicateOptions opts "Successfully execute single contract"
        assertNoFailedTransactions successfullyExecuteSingleContract
    , checkPredicateOptions opts "Successfully execute multiple contracts from same address and policy"
        assertNoFailedTransactions successfullyExecuteMultipleContractsFromSameAddress
    , checkPredicateOptions opts "Successfully execute multiple contracts from different addresses but same policy"
        assertNoFailedTransactions successfullyExecuteMultipleContractsFromDifferentAddressesButSamePolicy
    , checkPredicateOptions lenientOpts "Successfully execute multiple contracts from different addresses and policies"
        assertNoFailedTransactions successfullyExecuteMultipleContractsFromDifferentAddressesAndPolicy
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig successfullyExecuteSingleContract