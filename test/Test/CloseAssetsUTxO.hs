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

module Test.CloseAssetsUTxO
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
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash)
import Ledger.TimeSlot

import Test.Common
import CardanoOptions

-------------------------------------------------
-- Close Assets UTxO Scenarios
-------------------------------------------------
successfullyCloseSingleAssetsUTxO :: EmulatorTrace ()
successfullyCloseSingleAssetsUTxO = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
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
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = addr
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

  callEndpoint @"close-assets-utxo(s)" h1 $
    CloseAssetsParams
      { closeAssetsBeaconsBurned = [("Assets",-1)]
      , closeAssetsBeaconRedeemer = BurnBeacons
      , closeAssetsBeaconPolicy = optionsBeaconPolicy
      , closeAssetsOptionsVal = optionsValidator
      , closeAssetsOptionsAddress = addr
      , closeAssetsSpecificUTxOs = 
          [ ( assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      }

successfullyCloseMultipleAssetsUTxO :: EmulatorTrace ()
successfullyCloseMultipleAssetsUTxO = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
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
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = addr
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

  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = addr
      , assetsInfo = 
          [ ( Just assetsDatum{currentAssetQuantity = 50_000_000}
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 50_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 4

  callEndpoint @"close-assets-utxo(s)" h1 $
    CloseAssetsParams
      { closeAssetsBeaconsBurned = [("Assets",-2)]
      , closeAssetsBeaconRedeemer = BurnBeacons
      , closeAssetsBeaconPolicy = optionsBeaconPolicy
      , closeAssetsOptionsVal = optionsValidator
      , closeAssetsOptionsAddress = addr
      , closeAssetsSpecificUTxOs = 
          [ ( assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          , ( assetsDatum{currentAssetQuantity = 50_000_000}
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 50_000_000
            )
          ]
      }

utxoDatumNotAssetsForContract :: EmulatorTrace ()
utxoDatumNotAssetsForContract = do
  h1 <- activateContractWallet (knownWallet 2) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 2)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySymbol "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-assets-utxo(s)" h1 $
    CloseAssetsParams
      { closeAssetsBeaconsBurned = [("Assets",-1)]
      , closeAssetsBeaconRedeemer = BurnBeacons
      , closeAssetsBeaconPolicy = optionsBeaconPolicy
      , closeAssetsOptionsVal = optionsValidator
      , closeAssetsOptionsAddress = addr
      , closeAssetsSpecificUTxOs = 
          [ ( proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySymbol "Proposed" 1
           )
          ]
      }

singleAssetsBeaconNotBurned :: EmulatorTrace ()
singleAssetsBeaconNotBurned = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
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
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = addr
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

  callEndpoint @"close-assets-utxo(s)" h1 $
    CloseAssetsParams
      { closeAssetsBeaconsBurned = []
      , closeAssetsBeaconRedeemer = BurnBeacons
      , closeAssetsBeaconPolicy = optionsBeaconPolicy
      , closeAssetsOptionsVal = optionsValidator
      , closeAssetsOptionsAddress = addr
      , closeAssetsSpecificUTxOs = 
          [ ( assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      }

atLeastOneAssetsBeaconNotBurned :: EmulatorTrace ()
atLeastOneAssetsBeaconNotBurned = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
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
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = addr
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

  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = [("Assets",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = addr
      , assetsInfo = 
          [ ( Just assetsDatum{currentAssetQuantity = 50_000_000}
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 50_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 4

  callEndpoint @"close-assets-utxo(s)" h1 $
    CloseAssetsParams
      { closeAssetsBeaconsBurned = [("Assets",-1)]
      , closeAssetsBeaconRedeemer = BurnBeacons
      , closeAssetsBeaconPolicy = optionsBeaconPolicy
      , closeAssetsOptionsVal = optionsValidator
      , closeAssetsOptionsAddress = addr
      , closeAssetsSpecificUTxOs = 
          [ ( assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          , ( assetsDatum{currentAssetQuantity = 50_000_000}
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 50_000_000
            )
          ]
      }

activeBeaconMinted :: EmulatorTrace ()
activeBeaconMinted = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
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
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = addr
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

  callEndpoint @"close-assets-utxo(s)" h1 $
    CloseAssetsParams
      { closeAssetsBeaconsBurned = [("Assets",-1),("Active",1)]
      , closeAssetsBeaconRedeemer = BurnBeacons
      , closeAssetsBeaconPolicy = optionsBeaconPolicy
      , closeAssetsOptionsVal = optionsValidator
      , closeAssetsOptionsAddress = addr
      , closeAssetsSpecificUTxOs = 
          [ ( assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      }

stakingCredDidNotApprove :: EmulatorTrace ()
stakingCredDidNotApprove = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 4) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
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
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = addr
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

  callEndpoint @"close-assets-utxo(s)" h2 $
    CloseAssetsParams
      { closeAssetsBeaconsBurned = [("Assets",-1)]
      , closeAssetsBeaconRedeemer = BurnBeacons
      , closeAssetsBeaconPolicy = optionsBeaconPolicy
      , closeAssetsOptionsVal = optionsValidator
      , closeAssetsOptionsAddress = addr
      , closeAssetsSpecificUTxOs = 
          [ ( assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySymbol "Assets" 1
           <> lovelaceValueOf 100_000_000
            )
          ]
      }

successfullyCloseInvalidAssetsUTxO :: EmulatorTrace ()
successfullyCloseInvalidAssetsUTxO = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"create-assets-utxo" h1 $
    AssetsParams
      { assetsBeaconsMinted = []
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy
      , assetsAddress = addr
      , assetsInfo = 
          [ ( Just assetsDatum
            , lovelaceValueOf 5_000_000 
           <> lovelaceValueOf 100_000_000
            )
          ]
      , assetsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-assets-utxo(s)" h1 $
    CloseAssetsParams
      { closeAssetsBeaconsBurned = []
      , closeAssetsBeaconRedeemer = BurnBeacons
      , closeAssetsBeaconPolicy = optionsBeaconPolicy
      , closeAssetsOptionsVal = optionsValidator
      , closeAssetsOptionsAddress = addr
      , closeAssetsSpecificUTxOs = 
          [ ( assetsDatum
            , lovelaceValueOf 5_000_000 
           <> lovelaceValueOf 100_000_000
            )
          ]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Close Assets UTxO(s)"
    [ checkPredicateOptions opts "Fail if the datum is not an AssetsForContract datum"
        (Test.not assertNoFailedTransactions) utxoDatumNotAssetsForContract
    , checkPredicateOptions opts "Fail if only Assets beacon is not burned"
        (Test.not assertNoFailedTransactions) singleAssetsBeaconNotBurned
    , checkPredicateOptions opts "Fail if not all Assets beacons burned"
        (Test.not assertNoFailedTransactions) atLeastOneAssetsBeaconNotBurned
    , checkPredicateOptions opts "Fail if any Active beacons minted in tx"
        (Test.not assertNoFailedTransactions) activeBeaconMinted
    , checkPredicateOptions opts "Fail if staking credential did not approve"
        (Test.not assertNoFailedTransactions) stakingCredDidNotApprove

    , checkPredicateOptions opts "Successfully close a single Assets UTxO"
        assertNoFailedTransactions successfullyCloseSingleAssetsUTxO
    , checkPredicateOptions opts "Successfully close multiple Assets UTxOs"
        assertNoFailedTransactions successfullyCloseMultipleAssetsUTxO
    , checkPredicateOptions opts "Successfully close invalid Assets UTxO"
        assertNoFailedTransactions successfullyCloseInvalidAssetsUTxO
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig successfullyCloseInvalidAssetsUTxO