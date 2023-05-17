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

module Test.UpdateAddress
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
-- Update Address Scenarios
-------------------------------------------------
successfullyUpdateAddress :: EmulatorTrace ()
successfullyUpdateAddress = do
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
      creatorAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash
                                               $ mockWalletPaymentPubKeyHash
                                               $ knownWallet 2) Nothing
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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

  let newAddress = Address (PubKeyCredential $ unPaymentPubKeyHash
                                             $ mockWalletPaymentPubKeyHash
                                             $ knownWallet 3) 
                           Nothing

  callEndpoint @"update-address" h1 $
    AddressUpdateParams
      { updateAddressOptionsVal = optionsValidator
      , updateAddressOptionsAddress = optionsAddr1
      , updateAddressSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressRedeemer = UpdateAddress newAddress
      , updateAddressChangeAddress = optionsAddr1
      , updateAddressChange =
          [ ( Just activeDatum1{creatorAddress = newAddress}
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressAsInline = True
      }

addressUsesScriptHash :: EmulatorTrace ()
addressUsesScriptHash = do
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
      creatorAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash
                                               $ mockWalletPaymentPubKeyHash
                                               $ knownWallet 2) Nothing
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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

  let newAddress = Address (ScriptCredential alwaysSucceedValidatorHash) 
                           Nothing

  callEndpoint @"update-address" h1 $
    AddressUpdateParams
      { updateAddressOptionsVal = optionsValidator
      , updateAddressOptionsAddress = optionsAddr1
      , updateAddressSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressRedeemer = UpdateAddress newAddress
      , updateAddressChangeAddress = optionsAddr1
      , updateAddressChange =
          [ ( Just activeDatum1{creatorAddress = newAddress}
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressAsInline = True
      }

addressDoesntMatchRedeemer :: EmulatorTrace ()
addressDoesntMatchRedeemer = do
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
      creatorAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash
                                               $ mockWalletPaymentPubKeyHash
                                               $ knownWallet 2) Nothing
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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

  let newAddress = Address (PubKeyCredential $ unPaymentPubKeyHash
                                             $ mockWalletPaymentPubKeyHash
                                             $ knownWallet 3) 
                           Nothing

  callEndpoint @"update-address" h1 $
    AddressUpdateParams
      { updateAddressOptionsVal = optionsValidator
      , updateAddressOptionsAddress = optionsAddr1
      , updateAddressSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressRedeemer = UpdateAddress newAddress
      , updateAddressChangeAddress = optionsAddr1
      , updateAddressChange =
          [ ( Just activeDatum1{creatorAddress = creatorAddr2}
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressAsInline = True
      }

noOutputToAddress :: EmulatorTrace ()
noOutputToAddress = do
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
      creatorAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash
                                               $ mockWalletPaymentPubKeyHash
                                               $ knownWallet 2) Nothing
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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

  let newAddress = Address (PubKeyCredential $ unPaymentPubKeyHash
                                             $ mockWalletPaymentPubKeyHash
                                             $ knownWallet 3) 
                           Nothing

  callEndpoint @"update-address" h1 $
    AddressUpdateParams
      { updateAddressOptionsVal = optionsValidator
      , updateAddressOptionsAddress = optionsAddr1
      , updateAddressSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressRedeemer = UpdateAddress newAddress
      , updateAddressChangeAddress = optionsAddr1
      , updateAddressChange =
          []
      , updateAddressAsInline = True
      }

newOutputDatumNotInline :: EmulatorTrace ()
newOutputDatumNotInline = do
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
      creatorAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash
                                               $ mockWalletPaymentPubKeyHash
                                               $ knownWallet 2) Nothing
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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

  let newAddress = Address (PubKeyCredential $ unPaymentPubKeyHash
                                             $ mockWalletPaymentPubKeyHash
                                             $ knownWallet 3) 
                           Nothing

  callEndpoint @"update-address" h1 $
    AddressUpdateParams
      { updateAddressOptionsVal = optionsValidator
      , updateAddressOptionsAddress = optionsAddr1
      , updateAddressSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressRedeemer = UpdateAddress newAddress
      , updateAddressChangeAddress = optionsAddr1
      , updateAddressChange =
          [ ( Just activeDatum1{creatorAddress = newAddress}
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressAsInline = False
      }

valueChanged :: EmulatorTrace ()
valueChanged = do
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
      creatorAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash
                                               $ mockWalletPaymentPubKeyHash
                                               $ knownWallet 2) Nothing
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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

  let newAddress = Address (PubKeyCredential $ unPaymentPubKeyHash
                                             $ mockWalletPaymentPubKeyHash
                                             $ knownWallet 3) 
                           Nothing

  callEndpoint @"update-address" h1 $
    AddressUpdateParams
      { updateAddressOptionsVal = optionsValidator
      , updateAddressOptionsAddress = optionsAddr1
      , updateAddressSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressRedeemer = UpdateAddress newAddress
      , updateAddressChangeAddress = optionsAddr1
      , updateAddressChange =
          [ ( Just activeDatum1{creatorAddress = newAddress}
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 10_000_000
            )
          ]
      , updateAddressAsInline = True
      }

otherFieldChanged :: EmulatorTrace ()
otherFieldChanged = do
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
      creatorAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash
                                               $ mockWalletPaymentPubKeyHash
                                               $ knownWallet 2) Nothing
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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

  let newAddress = Address (PubKeyCredential $ unPaymentPubKeyHash
                                             $ mockWalletPaymentPubKeyHash
                                             $ knownWallet 3) 
                           Nothing

  callEndpoint @"update-address" h1 $
    AddressUpdateParams
      { updateAddressOptionsVal = optionsValidator
      , updateAddressOptionsAddress = optionsAddr1
      , updateAddressSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressRedeemer = UpdateAddress newAddress
      , updateAddressChangeAddress = optionsAddr1
      , updateAddressChange =
          [ ( Just activeDatum1{creatorAddress = newAddress,desiredAsset = testToken2}
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressAsInline = True
      }

stakinCredDidNotApprove :: EmulatorTrace ()
stakinCredDidNotApprove = do
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
      creatorAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash
                                               $ mockWalletPaymentPubKeyHash
                                               $ knownWallet 2) Nothing
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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

  let newAddress = Address (PubKeyCredential $ unPaymentPubKeyHash
                                             $ mockWalletPaymentPubKeyHash
                                             $ knownWallet 3) 
                           Nothing

  callEndpoint @"update-address" h2 $
    AddressUpdateParams
      { updateAddressOptionsVal = optionsValidator
      , updateAddressOptionsAddress = optionsAddr1
      , updateAddressSpecificUTxOs =
          [ ( activeDatum1
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressRedeemer = UpdateAddress newAddress
      , updateAddressChangeAddress = optionsAddr1
      , updateAddressChange =
          [ ( Just activeDatum1{creatorAddress = newAddress}
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          ]
      , updateAddressAsInline = True
      }

activeBeaconMissing :: EmulatorTrace ()
activeBeaconMissing = do
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

  let newAddress = Address (PubKeyCredential $ unPaymentPubKeyHash
                                             $ mockWalletPaymentPubKeyHash
                                             $ knownWallet 3) 
                           Nothing

  callEndpoint @"update-address" h1 $
    AddressUpdateParams
      { updateAddressOptionsVal = optionsValidator
      , updateAddressOptionsAddress = addr
      , updateAddressSpecificUTxOs =
          [ ( proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , updateAddressRedeemer = UpdateAddress newAddress
      , updateAddressChangeAddress = addr
      , updateAddressChange =
          [ ( Just proposeDatum{creatorAddress = newAddress}
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , updateAddressAsInline = True
      }

updateMultipleActiveUTxOs :: EmulatorTrace ()
updateMultipleActiveUTxOs = do
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
      creatorAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                               $ mockWalletPaymentPubKeyHash
                                               $ knownWallet 2) 
                             Nothing
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

  let newAddress = Address (PubKeyCredential $ unPaymentPubKeyHash
                                             $ mockWalletPaymentPubKeyHash
                                             $ knownWallet 3) 
                           Nothing

  callEndpoint @"update-address" h1 $
    AddressUpdateParams
      { updateAddressOptionsVal = optionsValidator
      , updateAddressOptionsAddress = optionsAddr1
      , updateAddressSpecificUTxOs =
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
      , updateAddressRedeemer = UpdateAddress newAddress
      , updateAddressChangeAddress = optionsAddr1
      , updateAddressChange =
          [ ( Just activeDatum1{creatorAddress = newAddress}
            , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId1 1
            <> lovelaceValueOf 100_000_000
            )
          , ( Just activeDatum2{creatorAddress = newAddress}
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySym1 "Active" 1
            <> singleton optionsBeaconPolicySym1 targetId2 1
            <> lovelaceValueOf 50_000_000
              )
          ]
      , updateAddressAsInline = True
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Address Updates"
    [ checkPredicateOptions opts "Fail if new address uses payment script"
        (Test.not assertNoFailedTransactions) addressUsesScriptHash
    , checkPredicateOptions opts "Fail if new address doesn't match redeemer address"
        (Test.not assertNoFailedTransactions) addressDoesntMatchRedeemer
    , checkPredicateOptions opts "Fail if no new output to options address"
        (Test.not assertNoFailedTransactions) noOutputToAddress
    , checkPredicateOptions opts "Fail if new datum not inline"
        (Test.not assertNoFailedTransactions) newOutputDatumNotInline
    , checkPredicateOptions opts "Fail if UTxO value changed"
        (Test.not assertNoFailedTransactions) valueChanged
    , checkPredicateOptions opts "Fail if other datum field changed"
        (Test.not assertNoFailedTransactions) otherFieldChanged
    , checkPredicateOptions opts "Fail if staking credential did not approve"
        (Test.not assertNoFailedTransactions) stakinCredDidNotApprove
    , checkPredicateOptions opts "Fail if active beacon missing"
        (Test.not assertNoFailedTransactions) activeBeaconMissing
    , checkPredicateOptions opts "Fail if multiple Active UTxOs updated in single tx"
        (Test.not assertNoFailedTransactions) updateMultipleActiveUTxOs

    , checkPredicateOptions opts "Successfully update creatorAddress of active contract"
        assertNoFailedTransactions successfullyUpdateAddress
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig updateMultipleActiveUTxOs