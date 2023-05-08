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
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}

module Test.Common where

import qualified Data.Map as Map
import Control.Lens hiding (from,index,to)
import Data.Default
import Data.Void (Void)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding (singleton,mintingPolicyHash)
import Ledger.Constraints as Constraints
import qualified Ledger.Constraints.TxConstraints as Constraints
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Ledger.Value (singleton)
import Ledger.Ada (lovelaceValueOf)
import Plutus.Script.Utils.V2.Scripts as UScripts
import Plutus.Trace
import Wallet.Emulator.Wallet
import Data.List (foldl')
import Prelude as Haskell (Semigroup (..), String)
import Cardano.Api.Shelley (ExecutionUnits (..),ProtocolParameters (..))
import Ledger.Tx.Internal as I
import Plutus.Script.Utils.V2.Generators (alwaysSucceedPolicy)

import CardanoOptions

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
txIdWithValue :: Value -> EmulatorTrace TxId
txIdWithValue value' = do
  state <- chainState
  let xs = Map.toList $ getIndex (state ^. index)
      findTxId v ((TxOutRef txId' _,o):ys)
        | I.txOutValue o == v = txId'
        | otherwise = findTxId v ys
  return $ findTxId value' xs

toRedeemer :: PlutusTx.ToData a => a -> Redeemer
toRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

mustPayToAddressWith :: Address -> Maybe (TxOutDatum Datum) -> Value -> TxConstraints i o
mustPayToAddressWith addr maybeDatum val =
  Constraints.singleton $ MustPayToAddress addr maybeDatum Nothing val

instance ToJSON OptionsDatum
instance FromJSON OptionsDatum

instance ToJSON OptionsRedeemer
instance FromJSON OptionsRedeemer

instance ToJSON OptionsBeaconRedeemer
instance FromJSON OptionsBeaconRedeemer

-------------------------------------------------
-- Params
-------------------------------------------------
data AssetsParams = AssetsParams
  { assetsBeaconsMinted :: [(TokenName,Integer)]
  , assetsBeaconRedeemer :: OptionsBeaconRedeemer
  , assetsBeaconPolicy :: MintingPolicy
  , assetsAddress :: Address
  , assetsInfo :: [(Maybe OptionsDatum, Value)]
  , assetsAsInline :: Bool
  } deriving (Generic,ToJSON,FromJSON)

data ProposeParams = ProposeParams
  { proposeBeaconsMinted :: [(TokenName,Integer)]
  , proposeBeaconRedeemer :: OptionsBeaconRedeemer
  , proposeBeaconPolicy :: MintingPolicy
  , proposeAddress :: Address
  , proposeInfo :: [(Maybe OptionsDatum, Value)]
  , proposeAsInline :: Bool
  } deriving (Generic,ToJSON,FromJSON)

data CloseAssetsParams = CloseAssetsParams
  { closeAssetsBeaconsBurned :: [[(TokenName,Integer)]]
  , closeAssetsBeaconRedeemer :: OptionsBeaconRedeemer
  , closeAssetsBeaconPolicies :: [MintingPolicy]
  , closeAssetsOptionsVal :: Validator
  , closeAssetsOptionsAddress :: Address
  , closeAssetsSpecificUTxOs :: [(OptionsDatum,Value)]
  } deriving (Generic,ToJSON,FromJSON)

data CloseProposalParams = CloseProposalParams
  { closeProposalBeaconsBurned :: [[(TokenName,Integer)]]
  , closeProposalBeaconRedeemer :: OptionsBeaconRedeemer
  , closeProposalBeaconPolicies :: [MintingPolicy]
  , closeProposalOptionsVal :: Validator
  , closeProposalOptionsAddress :: Address
  , closeProposalSpecificUTxOs :: [(OptionsDatum,Value)]
  } deriving (Generic,ToJSON,FromJSON)

data MultiAcceptParams = MultiAcceptParams
  { multiAcceptBeaconsMinted :: [[(TokenName,Integer)]]
  , multiAcceptBeaconRedeemer :: OptionsBeaconRedeemer
  , multiAcceptBeaconPolicies :: [MintingPolicy]
  , multiAcceptOptionsVal :: Validator
  , multiAcceptOptionsAddresses :: [Address]
  , multiAcceptSpecificUTxOs :: [[(OptionsDatum,Value)]]
  , multiAcceptChangeAddresses :: [Address]
  , multiAcceptChangeOutputs :: [[(Maybe OptionsDatum,Value)]]
  , multiAcceptPremiumAddresses :: [Address]
  , multiAcceptPremiumOutput :: [[(Maybe OptionsDatum,Value)]]
  , multiAcceptDatumAsInline :: Bool
  , multiAcceptWithTTL :: Bool
  } deriving (Generic,ToJSON,FromJSON)

data ExecuteParams = ExecuteParams
  { executeBeaconsBurned :: [[(TokenName,Integer)]]
  , executeBeaconRedeemer :: OptionsBeaconRedeemer
  , executeBeaconPolicies :: [MintingPolicy]
  , executeVal :: Validator
  , executeAddresses :: [Address]
  , executeContractIds :: [TokenName]
  , executeSpecificUTxOs :: [[(OptionsDatum,Value)]]
  , executeWithTTE :: Bool
  , executeCreatorAddresses :: [Address]
  , executeCreatorPayments :: [[(Maybe OptionsDatum,Value)]]
  } deriving (Generic,ToJSON,FromJSON)

data ClaimParams = ClaimParams
  { claimBeaconsBurned :: [(TokenName,Integer)]
  , claimBeaconRedeemer :: OptionsBeaconRedeemer
  , claimBeaconPolicy :: MintingPolicy
  , claimVal :: Validator
  , claimAddress :: Address
  , claimSpecificUTxOs :: [(OptionsDatum,Value)]
  , claimWithTTL :: Bool
  } deriving (Generic,ToJSON,FromJSON)

type TraceSchema =
      Endpoint "create-assets-utxo" AssetsParams
  .\/ Endpoint "propose-contract(s)" ProposeParams
  .\/ Endpoint "close-assets-utxo(s)" CloseAssetsParams
  .\/ Endpoint "close-proposal-utxo(s)" CloseProposalParams
  .\/ Endpoint "accept-contract(s)" MultiAcceptParams
  .\/ Endpoint "execute-contract(s)" ExecuteParams
  .\/ Endpoint "close-expired-contract(s)" ClaimParams

-------------------------------------------------
-- Configs
-------------------------------------------------
testToken1 :: (CurrencySymbol,TokenName)
testToken1 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken1")

testToken2 :: (CurrencySymbol,TokenName)
testToken2 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken2")

testToken3 :: (CurrencySymbol,TokenName)
testToken3 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken3")

testToken4 :: (CurrencySymbol,TokenName)
testToken4 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken4")

optionsBeaconPolicy1 :: MintingPolicy
optionsBeaconPolicy1 = optionsBeaconPolicy $ OptionsConfig (adaSymbol,adaToken) testToken1

optionsBeaconPolicy2 :: MintingPolicy
optionsBeaconPolicy2 = optionsBeaconPolicy $ OptionsConfig (adaSymbol,adaToken) testToken2

optionsBeaconPolicy3 :: MintingPolicy
optionsBeaconPolicy3 = optionsBeaconPolicy $ OptionsConfig (adaSymbol,adaToken) testToken3

optionsBeaconPolicy4 :: MintingPolicy
optionsBeaconPolicy4 = optionsBeaconPolicy $ OptionsConfig testToken1 testToken2

optionsBeaconPolicySym1 :: CurrencySymbol
optionsBeaconPolicySym1 = UScripts.scriptCurrencySymbol optionsBeaconPolicy1

optionsBeaconPolicySym2 :: CurrencySymbol
optionsBeaconPolicySym2 = UScripts.scriptCurrencySymbol optionsBeaconPolicy2

optionsBeaconPolicySym3 :: CurrencySymbol
optionsBeaconPolicySym3 = UScripts.scriptCurrencySymbol optionsBeaconPolicy3

optionsBeaconPolicySym4 :: CurrencySymbol
optionsBeaconPolicySym4 = UScripts.scriptCurrencySymbol optionsBeaconPolicy4


emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ Map.fromList wallets) def
  where
    user1 :: Value
    user1 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000

    user2 :: Value
    user2 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> singleton optionsBeaconPolicySym1 "Assets" 5
    
    user3 :: Value
    user3 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> singleton optionsBeaconPolicySym1 "Proposed" 5
    
    user4 :: Value
    user4 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> singleton optionsBeaconPolicySym1 "Active" 5
    
    user5 :: Value
    user5 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000

    user6 :: Value
    user6 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
  
    wallets :: [(Wallet,Value)]
    wallets = 
      [ (knownWallet 1, user1)
      , (knownWallet 2, user2)
      , (knownWallet 3, user3)
      , (knownWallet 4, user4)
      , (knownWallet 5, user5)
      , (knownWallet 6, user6)
      ]

benchConfig :: EmulatorConfig
benchConfig = emConfig & params .~ params'
  where 
    params' :: Params
    params' = def{emulatorPParams = pParams'}

    pParams' :: PParams
    pParams' = pParamsFromProtocolParams protoParams

    protoParams :: ProtocolParameters
    protoParams = def{ protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 10000000000
                                                                        ,executionMemory = 5000000})
                    --  , protocolParamMaxTxSize = 12300
                     }

lenientConfig :: EmulatorConfig
lenientConfig = emConfig & params .~ params'
  where 
    params' :: Params
    params' = def{emulatorPParams = pParams'}

    pParams' :: PParams
    pParams' = pParamsFromProtocolParams protoParams

    protoParams :: ProtocolParameters
    protoParams = def{ protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 10000000000
                                                                        ,executionMemory = 14000000})
                     , protocolParamMaxTxSize = 21300
                     }

-------------------------------------------------
-- Trace Models
-------------------------------------------------
createAssetsUTxO :: AssetsParams -> Contract () TraceSchema Text ()
createAssetsUTxO AssetsParams{..} = do
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  let beaconPolicyHash = mintingPolicyHash assetsBeaconPolicy
      beaconRedeemer = toRedeemer assetsBeaconRedeemer
      
      toDatum'
        | assetsAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum
      
      lookups = plutusV2MintingPolicy assetsBeaconPolicy
      
      tx' =
        -- | Mint Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          assetsBeaconsMinted
        )
        -- | Add assets
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith assetsAddress (fmap toDatum' d) v)
              mempty
              assetsInfo
           )
        -- | Must be signed by receiving pubkey
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Assets UTxO created"

proposeContracts :: ProposeParams -> Contract () TraceSchema Text ()
proposeContracts ProposeParams{..} = do
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  let beaconPolicyHash = mintingPolicyHash proposeBeaconPolicy
      beaconRedeemer = toRedeemer proposeBeaconRedeemer
      
      toDatum'
        | proposeAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum
      
      lookups = plutusV2MintingPolicy proposeBeaconPolicy
      
      tx' =
        -- | Mint Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          proposeBeaconsMinted
        )
        -- | Add contracts
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith proposeAddress (fmap toDatum' d) v)
              mempty
              proposeInfo
           )
        -- | Must be signed by receiving pubkey
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Proposal UTxO(s) created"

closeAssetsUTxO :: CloseAssetsParams -> Contract () TraceSchema Text ()
closeAssetsUTxO CloseAssetsParams{..} = do
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  assetsUtxos <- utxosAt closeAssetsOptionsAddress

  let beaconPolicyHashes = map mintingPolicyHash closeAssetsBeaconPolicies
      beaconRedeemer = toRedeemer closeAssetsBeaconRedeemer

      closeRedeemer = toRedeemer CloseAssets

      lPolicies = foldl' (\a b -> a <> plutusV2MintingPolicy b) 
                         (plutusV2MintingPolicy alwaysSucceedPolicy)
                         closeAssetsBeaconPolicies
      
      lookups = lPolicies
             <> plutusV2OtherScript closeAssetsOptionsVal
             <> Constraints.unspentOutputs assetsUtxos
      
      tx' =
        -- | Burn Beacons
        (mconcat $ zipWith (\z b -> foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer z beaconRedeemer t i) 
          mempty
          b)
          beaconPolicyHashes
          closeAssetsBeaconsBurned
        )
        -- | Must spend all utxos to be closed
        <> foldl' (\a (d,v) -> 
                      a <>
                      mustSpendScriptOutputWithMatchingDatumAndValue 
                        (UScripts.validatorHash closeAssetsOptionsVal) 
                        (== toDatum d)
                        (==v) 
                        closeRedeemer
                  ) 
                  mempty 
                  closeAssetsSpecificUTxOs
        -- | Must be signed by address staking pubkey
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Closed Assets UTxO(s)"

closeProposalUTxO :: CloseProposalParams -> Contract () TraceSchema Text ()
closeProposalUTxO CloseProposalParams{..} = do
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  proposalUtxos <- utxosAt closeProposalOptionsAddress

  let beaconPolicyHashes = map mintingPolicyHash closeProposalBeaconPolicies
      beaconRedeemer = toRedeemer closeProposalBeaconRedeemer

      closeRedeemer = toRedeemer CloseProposedContracts
      
      lPolicies = foldl' (\a b -> a <> plutusV2MintingPolicy b) 
                         (plutusV2MintingPolicy alwaysSucceedPolicy)
                         closeProposalBeaconPolicies

      lookups = lPolicies
             <> plutusV2OtherScript closeProposalOptionsVal
             <> Constraints.unspentOutputs proposalUtxos
      
      tx' =
        -- | Burn Beacons
        (mconcat $ zipWith (\z b -> foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer z beaconRedeemer t i) 
          mempty
          b)
          beaconPolicyHashes
          closeProposalBeaconsBurned
        )
        -- | Must spend all utxos to be closed
        <> foldl' (\a (d,v) -> 
                      a <>
                      mustSpendScriptOutputWithMatchingDatumAndValue 
                        (UScripts.validatorHash closeProposalOptionsVal) 
                        (== toDatum d)
                        (==v) 
                        closeRedeemer
                  ) 
                  mempty 
                  closeProposalSpecificUTxOs
        -- | Must be signed by address staking pubkey
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Closed Proposal UTxO(s)"

multiAcceptContracts :: MultiAcceptParams -> Contract () TraceSchema Text ()
multiAcceptContracts MultiAcceptParams{..} = do
  offerUtxos <- Map.unions <$> mapM utxosAt multiAcceptOptionsAddresses
  (start,_) <- currentNodeClientTimeRange
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let beaconPolicyHashes = map mintingPolicyHash multiAcceptBeaconPolicies
      beaconRedeemer = toRedeemer multiAcceptBeaconRedeemer

      toDatum'
        | multiAcceptDatumAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum

      acceptRedeemer = toRedeemer AcceptContract

      lPolicies = foldl' (\a b -> a <> plutusV2MintingPolicy b) 
                         (plutusV2MintingPolicy alwaysSucceedPolicy)
                         multiAcceptBeaconPolicies

      lookups = Constraints.unspentOutputs offerUtxos
             <> plutusV2OtherScript multiAcceptOptionsVal
             <> lPolicies

      tx' =
        -- | Burn Beacons
        (mconcat $ zipWith (\z b -> foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer z beaconRedeemer t i) 
          mempty
          b)
          beaconPolicyHashes
          multiAcceptBeaconsMinted
        )
        -- | Must spend all utxos to be accepted
        <> ( mconcat $ map (foldl' (\a (d,v) -> 
                      a <>
                      mustSpendScriptOutputWithMatchingDatumAndValue 
                        (UScripts.validatorHash multiAcceptOptionsVal) 
                        (== toDatum d)
                        (==v) 
                        acceptRedeemer
                  ) 
                  mempty)
                  multiAcceptSpecificUTxOs
           )
        -- | Return change to options address
        <> (mconcat $ zipWith (\z b -> foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith z (fmap toDatum' d) v)
              mempty
              b)
              multiAcceptChangeAddresses
              multiAcceptChangeOutputs
           )
        -- | Pay the premiums
        <> (mconcat $ zipWith (\z b -> foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith z (fmap toDatum' d) v)
              mempty
              b)
              multiAcceptPremiumAddresses
              multiAcceptPremiumOutput
           )
        -- | Must tell script current time
        <> (if multiAcceptWithTTL
            then mustValidateIn (from start)
            else mempty)
        -- | Must be signed by borrower
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Contract accepted"

executeContract :: ExecuteParams -> Contract () TraceSchema Text ()
executeContract ExecuteParams{..} = do
  contractUTxOs <- Map.unions <$> mapM utxosAt executeAddresses
  (_,end) <- currentNodeClientTimeRange
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let beaconPolicyHashes = map mintingPolicyHash executeBeaconPolicies
      beaconRedeemer = toRedeemer executeBeaconRedeemer
      
      executeRedeemers = map (toRedeemer . ExecuteContract) executeContractIds

      toDatum' = TxOutDatumInline . toDatum

      lPolicies = foldl' (\a b -> a <> plutusV2MintingPolicy b) 
                         (plutusV2MintingPolicy alwaysSucceedPolicy)
                         executeBeaconPolicies

      lookups = Constraints.unspentOutputs contractUTxOs
             <> plutusV2OtherScript executeVal
             <> lPolicies
      
      tx' =
        -- | Burn Beacons
        (mconcat $ zipWith (\z b -> foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer z beaconRedeemer t i) 
          mempty
          b)
          beaconPolicyHashes
          executeBeaconsBurned
        )
        -- | Must spend all utxos to be repaid
        <> (mconcat $ 
              zipWith 
                (\x y -> foldl' (\a (d,v) -> 
                      a <>
                      mustSpendScriptOutputWithMatchingDatumAndValue 
                        (UScripts.validatorHash executeVal) 
                        (== toDatum d)
                        (==v)
                        x) 
                  mempty 
                  y
                )
                executeRedeemers
                executeSpecificUTxOs
           )
        -- | Pay the creator
        <> (mconcat $ zipWith (\z b -> foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith z (fmap toDatum' d) v)
              mempty
              b)
              executeCreatorAddresses
              executeCreatorPayments
           )
        -- | Must tell script current time
        <> (if executeWithTTE
            then mustValidateIn (to end)
            else mempty)
        -- | Must be signed by borrower
        <> mustBeSignedBy userPubKeyHash

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Contract(s) executed"

claim :: ClaimParams -> Contract () TraceSchema Text ()
claim ClaimParams{..} = do
  contractUTxOs <- utxosAt claimAddress
  (start,_) <- currentNodeClientTimeRange
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let beaconPolicyHash = mintingPolicyHash claimBeaconPolicy
      beaconRedeemer = toRedeemer claimBeaconRedeemer
      
      claimRedeemer = toRedeemer CloseExpiredContract

      lookups = Constraints.unspentOutputs contractUTxOs
             <> plutusV2OtherScript claimVal
             <> plutusV2MintingPolicy claimBeaconPolicy
      
      tx' =
        -- | Burn Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          claimBeaconsBurned
        )
        -- | Must spend all utxos to be repaid
        <> foldl' (\a (d,v) -> 
                      a <>
                      mustSpendScriptOutputWithMatchingDatumAndValue 
                        (UScripts.validatorHash claimVal) 
                        (== toDatum d)
                        (==v) 
                        claimRedeemer
                  ) 
                  mempty 
                  claimSpecificUTxOs
        -- | Must tell script current time
        <> (if claimWithTTL
            then mustValidateIn (from start)
            else mempty)
        -- | Must be signed by borrower
        <> mustBeSignedBy userPubKeyHash

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Claimed an expired contract"

-------------------------------------------------
-- Endpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    createAssetsUTxO' = endpoint @"create-assets-utxo" createAssetsUTxO
    proposeContracts' = endpoint @"propose-contract(s)" proposeContracts
    closeAssetsUTxO' = endpoint @"close-assets-utxo(s)" closeAssetsUTxO
    closeProposalUTxO' = endpoint @"close-proposal-utxo(s)" closeProposalUTxO
    multiAcceptContracts' = endpoint @"accept-contract(s)" multiAcceptContracts
    executeContract' = endpoint @"execute-contract(s)" executeContract
    claim' = endpoint @"close-expired-contract(s)" claim
    choices = 
      [ createAssetsUTxO'
      , proposeContracts'
      , closeAssetsUTxO'
      , closeProposalUTxO'
      , multiAcceptContracts'
      , executeContract'
      , claim'
      ]