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

module Test.CreateAssetsUTxO
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Ledger.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash)

import Test.Common
import CardanoOptions

-------------------------------------------------
-- Create Assets UTxO Scenarios
-------------------------------------------------
successfullyCreateAssetsUTxO :: EmulatorTrace ()
successfullyCreateAssetsUTxO = do
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

mintMoreThanOneAssetsBeacon :: EmulatorTrace ()
mintMoreThanOneAssetsBeacon = do
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
      { assetsBeaconsMinted = [("Assets",2)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy1
      , assetsAddress = addr
      , assetsInfo = 
          [ ( Just assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Assets" 2
           <> lovelaceValueOf 100_000_000
           )
          ]
      , assetsAsInline = True
      }

mintTokenWithDifferentName :: EmulatorTrace ()
mintTokenWithDifferentName = do
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
      { assetsBeaconsMinted = [("Asts",1)]
      , assetsBeaconRedeemer = MintAssetsBeacon
      , assetsBeaconPolicy = optionsBeaconPolicy1
      , assetsAddress = addr
      , assetsInfo = 
          [ ( Just assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym1 "Asts" 1
           <> lovelaceValueOf 100_000_000
           )
          ]
      , assetsAsInline = True
      }

mintAdditionalTokens :: EmulatorTrace ()
mintAdditionalTokens = do
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
      { assetsBeaconsMinted = [("Assets",1),("Other",1)]
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

burnOtherTokens :: EmulatorTrace ()
burnOtherTokens = do
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
      { assetsBeaconsMinted = [("Assets",1),("Proposed",-1)]
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

beaconGoesToNonDappAddress :: EmulatorTrace ()
beaconGoesToNonDappAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        }
      addr = Address (ScriptCredential alwaysSucceedValidatorHash)
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

beaconGoesToNonStakingDappAddress :: EmulatorTrace ()
beaconGoesToNonStakingDappAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     Nothing
  
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

receivingAddressDidNotApprove :: EmulatorTrace ()
receivingAddressDidNotApprove = do
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
                           $ knownWallet 2)
  
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

datumHasWrongBeaconSymbol :: EmulatorTrace ()
datumHasWrongBeaconSymbol = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = adaSymbol
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

datumHasWrongCurrentAsset :: EmulatorTrace ()
datumHasWrongCurrentAsset = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = testToken1
        , currentAssetQuantity = 10
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

datumHasNegativeQuantity :: EmulatorTrace ()
datumHasNegativeQuantity = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = -100_000_000
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

datumHasWrongDesiredAsset :: EmulatorTrace ()
datumHasWrongDesiredAsset = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken2
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

datumIsNotAssetsForContract :: EmulatorTrace ()
datumIsNotAssetsForContract = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = 0
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

datumIsNotInline :: EmulatorTrace ()
datumIsNotInline = do
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
      , assetsAsInline = False
      }

beaconStoredWithWrongCurrentAssetValue :: EmulatorTrace ()
beaconStoredWithWrongCurrentAssetValue = do
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
           <> lovelaceValueOf 10_000_000
           )
          ]
      , assetsAsInline = True
      }

beaconStoredWithAdditionalValue :: EmulatorTrace ()
beaconStoredWithAdditionalValue = do
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
           <> (uncurry singleton testToken1) 10
           )
          ]
      , assetsAsInline = True
      }

beaconNotStoredWithMinDeposit :: EmulatorTrace ()
beaconNotStoredWithMinDeposit = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym4
        , currentAsset = testToken1
        , currentAssetQuantity = 10
        , desiredAsset = testToken2
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
      , assetsBeaconPolicy = optionsBeaconPolicy4
      , assetsAddress = addr
      , assetsInfo = 
          [ ( Just assetsDatum
            , lovelaceValueOf 4_000_000 
           <> singleton optionsBeaconPolicySym4 "Assets" 1
           <> (uncurry singleton testToken1) 10
           )
          ]
      , assetsAsInline = True
      }

successfullyUseNonAdaAsset :: EmulatorTrace ()
successfullyUseNonAdaAsset = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym4
        , currentAsset = testToken1
        , currentAssetQuantity = 10
        , desiredAsset = testToken2
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
      , assetsBeaconPolicy = optionsBeaconPolicy4
      , assetsAddress = addr
      , assetsInfo = 
          [ ( Just assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym4 "Assets" 1
           <> (uncurry singleton testToken1) 10
           )
          ]
      , assetsAsInline = True
      }

wrongPairPolicyUsed :: EmulatorTrace ()
wrongPairPolicyUsed = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let assetsDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym4
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
      , assetsBeaconPolicy = optionsBeaconPolicy4
      , assetsAddress = addr
      , assetsInfo = 
          [ ( Just assetsDatum
            , lovelaceValueOf 5_000_000 
           <> singleton optionsBeaconPolicySym4 "Assets" 1
           <> lovelaceValueOf 100_000_000
           )
          ]
      , assetsAsInline = True
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Create An Assets UTxO"
    [ checkPredicateOptions opts "Fail if more than one Assets beacon minted in tx"
        (Test.not assertNoFailedTransactions) mintMoreThanOneAssetsBeacon
    , checkPredicateOptions opts "Fail if the minted token does not have the name 'Assets'"
        (Test.not assertNoFailedTransactions) mintTokenWithDifferentName
    , checkPredicateOptions opts "Fail if additional tokens minted with redeemer"
        (Test.not assertNoFailedTransactions) mintAdditionalTokens
    , checkPredicateOptions opts "Fail if additional tokens burned with this redeemer"
        (Test.not assertNoFailedTransactions) burnOtherTokens
    , checkPredicateOptions opts "Fail if beacon goes to a non dApp address"
        (Test.not assertNoFailedTransactions) beaconGoesToNonDappAddress
    , checkPredicateOptions opts "Fail if beacon goes to non-staking dapp address"
        (Test.not assertNoFailedTransactions) beaconGoesToNonStakingDappAddress
    , checkPredicateOptions opts "Fail if receiving staking credential did not approve"
        (Test.not assertNoFailedTransactions) receivingAddressDidNotApprove
    , checkPredicateOptions opts "Fail if AssetsForContract datum has wrong beacon symbol"
        (Test.not assertNoFailedTransactions) datumHasWrongBeaconSymbol
    , checkPredicateOptions opts "Fail if AssetsForContract datum has wrong currentAsset"
        (Test.not assertNoFailedTransactions) datumHasWrongCurrentAsset
    , checkPredicateOptions opts "Fail if AssetsForContract datum has negative quantity"
        (Test.not assertNoFailedTransactions) datumHasNegativeQuantity
    , checkPredicateOptions opts "Fail if AssetsForContract datum has wrong desiredAsset"
        (Test.not assertNoFailedTransactions) datumHasWrongDesiredAsset
    , checkPredicateOptions opts "Fail if beacon not stored with AssetsForContract datum"
        (Test.not assertNoFailedTransactions) datumIsNotAssetsForContract
    , checkPredicateOptions opts "Fail if datum is not inline datum"
        (Test.not assertNoFailedTransactions) datumIsNotInline
    , checkPredicateOptions opts "Fail if beacon not stored with proper amount of current asset"
        (Test.not assertNoFailedTransactions) beaconStoredWithWrongCurrentAssetValue
    , checkPredicateOptions opts "Fail if beacon stored with extra value"
        (Test.not assertNoFailedTransactions) beaconStoredWithAdditionalValue
    , checkPredicateOptions opts "Fail if beacon not stored with minimum ADA deposit"
        (Test.not assertNoFailedTransactions) beaconNotStoredWithMinDeposit
    , checkPredicateOptions opts "Fail if policy for different trading pair used"
        (Test.not assertNoFailedTransactions) wrongPairPolicyUsed

    , checkPredicateOptions opts "Successfully create Assets UTxO"
        assertNoFailedTransactions successfullyCreateAssetsUTxO
    , checkPredicateOptions opts "Successfully use non ADA asset for UTxO"
        assertNoFailedTransactions successfullyUseNonAdaAsset
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig wrongPairPolicyUsed