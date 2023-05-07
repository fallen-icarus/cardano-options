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

module Test.AcceptContract
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
-- Accept Scenarios
-------------------------------------------------
successfullyAcceptContract :: EmulatorTrace ()
successfullyAcceptContract = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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

successfullyUseMultiAcceptContract :: EmulatorTrace ()
successfullyUseMultiAcceptContract = do
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

  callEndpoint @"create-assets-utxo" h2 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = optionsAddr2
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
        , creatorAddress = creatorAddr2
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

  callEndpoint @"propose-contract(s)" h2 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy
      , proposeAddress = optionsAddr2
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
        , creatorAddress = creatorAddr2
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId2
        }
           
  callEndpoint @"multi-accept-contracts" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1),(targetId1,2)]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicy = optionsBeaconPolicy
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySymbol "Proposed" 1
              )
            , ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySymbol "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySymbol "Active" 1
            <> singleton optionsBeaconPolicySymbol targetId1 1
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

activeBeaconNotMinted :: EmulatorTrace ()
activeBeaconNotMinted = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId
        }
           
  callEndpoint @"accept-contract" h2 $
    AcceptParams
      { acceptBeaconsMinted = [("Activ",1),("Assets",-1),("Proposed",-1),(targetId,2)]
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
           <> singleton optionsBeaconPolicySymbol "Activ" 1
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

assetsBeaconNotBurned :: EmulatorTrace ()
assetsBeaconNotBurned = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId
        }
           
  callEndpoint @"accept-contract" h2 $
    AcceptParams
      { acceptBeaconsMinted = [("Active",1),("Proposed",-1),(targetId,2)]
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

proposedBeaconNotBurned :: EmulatorTrace ()
proposedBeaconNotBurned = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId
        }
           
  callEndpoint @"accept-contract" h2 $
    AcceptParams
      { acceptBeaconsMinted = [("Active",1),("Assets",-1),(targetId,2)]
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

multipleActiveBeaconsMinted :: EmulatorTrace ()
multipleActiveBeaconsMinted = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId
        }
           
  callEndpoint @"accept-contract" h2 $
    AcceptParams
      { acceptBeaconsMinted = [("Active",2),("Assets",-1),("Proposed",-1),(targetId,2)]
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

contractIDsNotMinted :: EmulatorTrace ()
contractIDsNotMinted = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId
        }
           
  callEndpoint @"accept-contract" h2 $
    AcceptParams
      { acceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1)]
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

additionalContractIDsMinted :: EmulatorTrace ()
additionalContractIDsMinted = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId
        }
           
  callEndpoint @"accept-contract" h2 $
    AcceptParams
      { acceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1),(targetId,3)]
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

additionalTokensMinted :: EmulatorTrace ()
additionalTokensMinted = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId
        }
           
  callEndpoint @"accept-contract" h2 $
    AcceptParams
      { acceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1),(targetId,2),("Other",1)]
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

inputsFromOtherDappAddresses :: EmulatorTrace ()
inputsFromOtherDappAddresses = do
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

  callEndpoint @"create-assets-utxo" h2 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = optionsAddr2
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
        , creatorAddress = creatorAddr2
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

  callEndpoint @"propose-contract(s)" h2 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy
      , proposeAddress = optionsAddr2
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
        , creatorAddress = creatorAddr2
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId2
        }
           
  callEndpoint @"multi-accept-contracts" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1),(targetId1,2)]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicy = optionsBeaconPolicy
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1,optionsAddr2]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySymbol "Proposed" 1
              )
            , ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySymbol "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            ]
          , [ ( proposeDatum2
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySymbol "Proposed" 1
              )
            , ( assetsDatum2
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySymbol "Assets" 1
             <> lovelaceValueOf 50_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySymbol "Active" 1
            <> singleton optionsBeaconPolicySymbol targetId1 1
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

additionalProposedInputFromDappAddress :: EmulatorTrace ()
additionalProposedInputFromDappAddress = do
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
        , creatorAddress = creatorAddr2
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
        , creatorAddress = creatorAddr2
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId2
        }
           
  callEndpoint @"multi-accept-contracts" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1),(targetId1,2)]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicy = optionsBeaconPolicy
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySymbol "Proposed" 1
              )
            , ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySymbol "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            , ( proposeDatum2
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySymbol "Proposed" 1
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySymbol "Active" 1
            <> singleton optionsBeaconPolicySymbol targetId1 1
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

additionalAssetsInputFromDappAddress :: EmulatorTrace ()
additionalAssetsInputFromDappAddress = do
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
        , creatorAddress = creatorAddr2
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
        , creatorAddress = creatorAddr2
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId2
        }
           
  callEndpoint @"multi-accept-contracts" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1),(targetId1,2)]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicy = optionsBeaconPolicy
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySymbol "Proposed" 1
              )
            , ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySymbol "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            , ( assetsDatum2
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySymbol "Assets" 1
             <> lovelaceValueOf 50_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySymbol "Active" 1
            <> singleton optionsBeaconPolicySymbol targetId1 1
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

missingProposedInput :: EmulatorTrace ()
missingProposedInput = do
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
        , creatorAddress = creatorAddr2
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
        , creatorAddress = creatorAddr2
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId2
        }
           
  callEndpoint @"multi-accept-contracts" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [("Active",1),("Assets",-1),(targetId1,2)]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicy = optionsBeaconPolicy
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySymbol "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySymbol "Active" 1
            <> singleton optionsBeaconPolicySymbol targetId1 1
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

missingAssetsInput :: EmulatorTrace ()
missingAssetsInput = do
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
        , creatorAddress = creatorAddr2
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
        , creatorAddress = creatorAddr2
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId2
        }
           
  callEndpoint @"multi-accept-contracts" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [("Active",1),("Proposed",-1),(targetId1,2)]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicy = optionsBeaconPolicy
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySymbol "Proposed" 1
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySymbol "Active" 1
            <> singleton optionsBeaconPolicySymbol targetId1 1
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

missingBothInputs :: EmulatorTrace ()
missingBothInputs = do
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
        , creatorAddress = creatorAddr2
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
        , creatorAddress = creatorAddr2
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId2
        }
           
  callEndpoint @"multi-accept-contracts" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [("Active",1),(targetId1,2)]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicy = optionsBeaconPolicy
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ []
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySymbol "Active" 1
            <> singleton optionsBeaconPolicySymbol targetId1 1
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

-- | This models the scenario where the assets and proposed inputs are correct,
-- and there are additional, unrelated inputs from the dApp address.
additionalNonSignificantInputFromAddress :: EmulatorTrace ()
additionalNonSignificantInputFromAddress = do
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
        , creatorAddress = creatorAddr2
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
        , creatorAddress = creatorAddr2
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId2
        }

  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = []
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy
      , proposeAddress = optionsAddr1
      , proposeInfo = 
          [ ( Just activeDatum1
            , lovelaceValueOf 3_000_000 
            )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 10
           
  callEndpoint @"multi-accept-contracts" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1),(targetId1,2)]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId1 optionsStakingCred1
      , multiAcceptBeaconPolicy = optionsBeaconPolicy
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySymbol "Proposed" 1
              )
            , ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySymbol "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            , ( activeDatum1
              , lovelaceValueOf 3_000_000 
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySymbol "Active" 1
            <> singleton optionsBeaconPolicySymbol targetId1 1
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

contractIdDoesntMatchAssetsUTxO :: EmulatorTrace ()
contractIdDoesntMatchAssetsUTxO = do
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

  callEndpoint @"create-assets-utxo" h2 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = optionsAddr2
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
        , creatorAddress = creatorAddr2
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

  callEndpoint @"propose-contract(s)" h2 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy
      , proposeAddress = optionsAddr2
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
        , creatorAddress = creatorAddr2
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId2
        }
           
  callEndpoint @"multi-accept-contracts" h2 $
    MultiAcceptParams
      { multiAcceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1),(targetId2,2)]
      , multiAcceptBeaconRedeemer = MintActiveBeacon targetId2 optionsStakingCred1
      , multiAcceptBeaconPolicy = optionsBeaconPolicy
      , multiAcceptOptionsVal = optionsValidator
      , multiAcceptOptionsAddresses = [optionsAddr1]
      , multiAcceptSpecificUTxOs = 
          [ [ ( proposeDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton optionsBeaconPolicySymbol "Proposed" 1
              )
            , ( assetsDatum1
              , lovelaceValueOf 5_000_000 
             <> singleton optionsBeaconPolicySymbol "Assets" 1
             <> lovelaceValueOf 100_000_000
              )
            ]
          ]
      , multiAcceptChangeAddresses = [optionsAddr1]
      , multiAcceptChangeOutputs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 5_000_000
            <> singleton optionsBeaconPolicySymbol "Active" 1
            <> singleton optionsBeaconPolicySymbol targetId2 1
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

differentBeaconSymbolsInTokenName :: EmulatorTrace ()
differentBeaconSymbolsInTokenName = do
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        }
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = alwaysSucceedPolicy
      , proposeAddress = optionsAddr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton (scriptCurrencySymbol alwaysSucceedPolicy) "Proposed" 1
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId
        }
           
  callEndpoint @"accept-contract" h2 $
    AcceptParams
      { acceptBeaconsMinted = [("Active",1),("Assets",-1),(targetId,2)]
      , acceptBeaconRedeemer = MintActiveBeacon targetId optionsStakingCred
      , acceptBeaconPolicy = optionsBeaconPolicy
      , acceptOptionsVal = optionsValidator
      , acceptOptionsAddress = optionsAddr
      , acceptSpecificUTxOs = 
          [ ( proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton (scriptCurrencySymbol alwaysSucceedPolicy) "Proposed" 1
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

differentCurrentAssets :: EmulatorTrace ()
differentCurrentAssets = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred = PubKeyCredential
                         $ unPaymentPubKeyHash
                         $ mockWalletPaymentPubKeyHash
                         $ knownWallet 1
      assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = testToken1
        , currentAssetQuantity = 10
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
           <> (uncurry singleton testToken1) 10
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
        , strikePrice = unsafeRatio 1 10
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
                             <> (uncurry singleton testToken1) 10
                              )

  let targetId = txIdAsToken targetHash
      activeDatum = ActiveContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
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
           <> (uncurry singleton testToken1) 10
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

differentCurrentAssetQuantity :: EmulatorTrace ()
differentCurrentAssetQuantity = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred = PubKeyCredential
                         $ unPaymentPubKeyHash
                         $ mockWalletPaymentPubKeyHash
                         $ knownWallet 1
      assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 50_000_000
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
           <> lovelaceValueOf 50_000_000
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
        , strikePrice = unsafeRatio 1 10
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
                             <> lovelaceValueOf 50_000_000
                              )

  let targetId = txIdAsToken targetHash
      activeDatum = ActiveContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
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
           <> lovelaceValueOf 50_000_000
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

wrongRedeemerStakingCredential :: EmulatorTrace ()
wrongRedeemerStakingCredential = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred = PubKeyCredential
                         $ unPaymentPubKeyHash
                         $ mockWalletPaymentPubKeyHash
                         $ knownWallet 1
      optionsStakingCred2 = PubKeyCredential
                         $ unPaymentPubKeyHash
                         $ mockWalletPaymentPubKeyHash
                         $ knownWallet 2
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = targetId
        }
           
  callEndpoint @"accept-contract" h2 $
    AcceptParams
      { acceptBeaconsMinted = [("Active",1),("Assets",-1),("Proposed",-1),(targetId,2)]
      , acceptBeaconRedeemer = MintActiveBeacon targetId optionsStakingCred2
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

minDepositNotPaidToCreator :: EmulatorTrace ()
minDepositNotPaidToCreator = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

premiumNotPaidToCreator :: EmulatorTrace ()
premiumNotPaidToCreator = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
            , lovelaceValueOf 3_000_000
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

premiumPaidToWrongAddress :: EmulatorTrace ()
premiumPaidToWrongAddress = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
      , acceptPremiumAddress = creatorAddr{addressStakingCredential = Just $ StakingHash optionsStakingCred}
      , acceptPremiumOutput = 
          [ ( Nothing
            ,  lovelaceValueOf 10_000_000
            <> lovelaceValueOf 3_000_000
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

premiumSplitOverMultipleOutputs :: EmulatorTrace ()
premiumSplitOverMultipleOutputs = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
            , lovelaceValueOf 10_000_000
            )
          , ( Nothing
            , lovelaceValueOf 3_000_000
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

missingOptionsOutput :: EmulatorTrace ()
missingOptionsOutput = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
          [
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

optionsOutputToDifferentDappAddress :: EmulatorTrace ()
optionsOutputToDifferentDappAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 5) endpoints

  let optionsStakingCred = PubKeyCredential
                         $ unPaymentPubKeyHash
                         $ mockWalletPaymentPubKeyHash
                         $ knownWallet 1
      optionsStakingCred2 = PubKeyCredential
                         $ unPaymentPubKeyHash
                         $ mockWalletPaymentPubKeyHash
                         $ knownWallet 2
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
      , acceptChangeAddress = optionsAddr{addressStakingCredential = Just $ StakingHash optionsStakingCred2}
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

multipleOutputsToDappAddress :: EmulatorTrace ()
multipleOutputsToDappAddress = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
          , ( Just activeDatum
            , lovelaceValueOf 5_000_000
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

activeOutputMissingMinDeposit :: EmulatorTrace ()
activeOutputMissingMinDeposit = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
            , singleton optionsBeaconPolicySymbol "Active" 1
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

activeOutputMissingCurrentAsset :: EmulatorTrace ()
activeOutputMissingCurrentAsset = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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

activeOutputMissingActiveBeacon :: EmulatorTrace ()
activeOutputMissingActiveBeacon = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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

activeOutputMissingContractId :: EmulatorTrace ()
activeOutputMissingContractId = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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

activeOutputHasBothContractIds :: EmulatorTrace ()
activeOutputHasBothContractIds = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
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
           <> singleton optionsBeaconPolicySymbol targetId 2
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

datumHasWrongBeaconSymbol :: EmulatorTrace ()
datumHasWrongBeaconSymbol = do
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
        , strikePrice = unsafeRatio 1 10
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
        { beaconSymbol = adaSymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
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

datumHasWrongCurrentAsset :: EmulatorTrace ()
datumHasWrongCurrentAsset = do
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
        , strikePrice = unsafeRatio 1 10
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
        , currentAsset = testToken1
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
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

datumHasWrongCurrentAssetQuantity :: EmulatorTrace ()
datumHasWrongCurrentAssetQuantity = do
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
        , strikePrice = unsafeRatio 1 10
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
        , currentAssetQuantity = 10_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
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

datumHasWrongDesiredAsset :: EmulatorTrace ()
datumHasWrongDesiredAsset = do
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
        , strikePrice = unsafeRatio 1 10
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
        , desiredAsset = testToken2
        , strikePrice = unsafeRatio 1 10
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

datumHasWrongStrikePrice :: EmulatorTrace ()
datumHasWrongStrikePrice = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 1
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

datumHasWrongCreatorAddress :: EmulatorTrace ()
datumHasWrongCreatorAddress = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = optionsAddr
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

datumHasWrongPremiumAsset :: EmulatorTrace ()
datumHasWrongPremiumAsset = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = testToken1
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

datumHasWrongPremium :: EmulatorTrace ()
datumHasWrongPremium = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 1_000_000
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

datumHasWrongExpiration :: EmulatorTrace ()
datumHasWrongExpiration = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 230
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

datumHasWrongContractId :: EmulatorTrace ()
datumHasWrongContractId = do
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
        , strikePrice = unsafeRatio 1 10
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
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = creatorAddr
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 20
        , contractId = "Active"
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

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Accept Contract"
    [ checkPredicateOptions opts "Fail if Active beacon not minted"
        (Test.not assertNoFailedTransactions) activeBeaconNotMinted
    , checkPredicateOptions opts "Fail if Assets beacon not burned"
        (Test.not assertNoFailedTransactions) assetsBeaconNotBurned
    , checkPredicateOptions opts "Fail if Proposed beacon not burned"
        (Test.not assertNoFailedTransactions) proposedBeaconNotBurned
    , checkPredicateOptions opts "Fail if multiple Active beacons minted"
        (Test.not assertNoFailedTransactions) multipleActiveBeaconsMinted
    , checkPredicateOptions opts "Fail if ContractIDs not minted"
        (Test.not assertNoFailedTransactions) contractIDsNotMinted
    , checkPredicateOptions opts "Fail if additional ContractIDs minted"
        (Test.not assertNoFailedTransactions) additionalContractIDsMinted
    , checkPredicateOptions opts "Fail if additional tokens minted"
        (Test.not assertNoFailedTransactions) additionalTokensMinted
    , checkPredicateOptions opts "Fail if there are inputs from other dApp addresses"
        (Test.not assertNoFailedTransactions) inputsFromOtherDappAddresses
    , checkPredicateOptions opts "Fail if there are more than one Proposed inputs from address"
        (Test.not assertNoFailedTransactions) additionalProposedInputFromDappAddress
    , checkPredicateOptions opts "Fail if there are more than one Assets inputs from address"
        (Test.not assertNoFailedTransactions) additionalAssetsInputFromDappAddress
    , checkPredicateOptions opts "Fail if Proposed UTxO is missing from inputs"
        (Test.not assertNoFailedTransactions) missingProposedInput
    , checkPredicateOptions opts "Fail if Assets UTxO is missing from inputs"
        (Test.not assertNoFailedTransactions) missingAssetsInput
    , checkPredicateOptions opts "Fail if both inputs missing from tx"
        (Test.not assertNoFailedTransactions) missingBothInputs
    , checkPredicateOptions opts "Fail if any invalid inputs found"
        (Test.not assertNoFailedTransactions) additionalNonSignificantInputFromAddress
    , checkPredicateOptions opts "Fail if ContractID does not match Assets input tx hash"
        (Test.not assertNoFailedTransactions) contractIdDoesntMatchAssetsUTxO
    , checkPredicateOptions opts "Fail if token uses different policy id"
        (Test.not assertNoFailedTransactions) differentBeaconSymbolsInTokenName
    , checkPredicateOptions opts "Fail if datums don't agree on currentAsset"
        (Test.not assertNoFailedTransactions) differentCurrentAssets
    , checkPredicateOptions opts "Fail if datums don't agree on currentAssetQuantity"
        (Test.not assertNoFailedTransactions) differentCurrentAssetQuantity
    , checkPredicateOptions opts "Fail if redeemer has wrong staking credential"
        (Test.not assertNoFailedTransactions) wrongRedeemerStakingCredential
    , checkPredicateOptions opts "Fail if Proposed UTxO min deposit not returned to creator"
        (Test.not assertNoFailedTransactions) minDepositNotPaidToCreator
    , checkPredicateOptions opts "Fail if premium not paid to creator"
        (Test.not assertNoFailedTransactions) premiumNotPaidToCreator
    , checkPredicateOptions opts "Fail if premium paid to wrong address"
        (Test.not assertNoFailedTransactions) premiumPaidToWrongAddress
    , checkPredicateOptions opts "Fail if premium split over multiple outputs to creator's address"
        (Test.not assertNoFailedTransactions) premiumSplitOverMultipleOutputs
    , checkPredicateOptions opts "Fail if no output to a dApp address"
        (Test.not assertNoFailedTransactions) missingOptionsOutput
    , checkPredicateOptions opts "Fail if output to wrong dApp address"
        (Test.not assertNoFailedTransactions) optionsOutputToDifferentDappAddress
    , checkPredicateOptions opts "Fail if multipleOutputsToDappAddress"
        (Test.not assertNoFailedTransactions) multipleOutputsToDappAddress
    , checkPredicateOptions opts "Fail if active output does not have min deposit"
        (Test.not assertNoFailedTransactions) activeOutputMissingMinDeposit
    , checkPredicateOptions opts "Fail if active output missing current asset amount"
        (Test.not assertNoFailedTransactions) activeOutputMissingCurrentAsset
    , checkPredicateOptions opts "Fail if active output missing active beacon"
        (Test.not assertNoFailedTransactions) activeOutputMissingActiveBeacon
    , checkPredicateOptions opts "Fail if active output missing ContractID"
        (Test.not assertNoFailedTransactions) activeOutputMissingContractId
    , checkPredicateOptions opts "Fail if active output has both ContractIDs"
        (Test.not assertNoFailedTransactions) activeOutputHasBothContractIds
    , checkPredicateOptions opts "Fail if ActiveContract datum has wrong beaconSymbol"
        (Test.not assertNoFailedTransactions) datumHasWrongBeaconSymbol
    , checkPredicateOptions opts "Fail if ActiveContract datum has wrong currentAsset"
        (Test.not assertNoFailedTransactions) datumHasWrongCurrentAsset
    , checkPredicateOptions opts "Fail if ActiveContract datum has wrong currentAssetQuantity"
        (Test.not assertNoFailedTransactions) datumHasWrongCurrentAssetQuantity
    , checkPredicateOptions opts "Fail if ActiveContract datum has wrong desiredAsset"
        (Test.not assertNoFailedTransactions) datumHasWrongDesiredAsset
    , checkPredicateOptions opts "Fail if ActiveContract datum has wrong strikePrice"
        (Test.not assertNoFailedTransactions) datumHasWrongStrikePrice
    , checkPredicateOptions opts "Fail if ActiveContract datum has wrong creatorAddress"
        (Test.not assertNoFailedTransactions) datumHasWrongCreatorAddress
    , checkPredicateOptions opts "Fail if ActiveContract datum has wrong premiumAsset"
        (Test.not assertNoFailedTransactions) datumHasWrongPremiumAsset
    , checkPredicateOptions opts "Fail if ActiveContract datum has wrong premium"
        (Test.not assertNoFailedTransactions) datumHasWrongPremium
    , checkPredicateOptions opts "Fail if ActiveContract datum has wrong expiration"
        (Test.not assertNoFailedTransactions) datumHasWrongExpiration
    , checkPredicateOptions opts "Fail if ActiveContract datum has wrong contractId"
        (Test.not assertNoFailedTransactions) datumHasWrongContractId
    
    , checkPredicateOptions opts "Successfully accept contract"
        assertNoFailedTransactions successfullyAcceptContract
    , checkPredicateOptions opts "Successfully use multiAccept model"
        assertNoFailedTransactions successfullyUseMultiAcceptContract
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig activeOutputHasBothContractIds