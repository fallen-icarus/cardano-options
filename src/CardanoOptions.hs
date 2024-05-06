{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module CardanoOptions
  ( -- * On-chain Datums
    ProposalDatum(..)
  , ActiveDatum(..)
  , PaymentDatum(..)

    -- * On-chain Redeemers
  , OptionsRedeemer(..)
  , ProposalBeaconsRedeemer(..)
  , ActiveBeaconsRedeemer(..)
  , AddressObserverRedeemer(..)

    -- * Contracts
  , proxyScript
  , proxyValidatorHash
  , optionsScript
  , optionsScriptHash
  , addressObserverScript
  , addressObserverScriptHash
  , activeBeaconScript
  , activeBeaconScriptHash
  , activeBeaconCurrencySymbol
  , proposalBeaconScript
  , proposalBeaconScriptHash
  , proposalBeaconCurrencySymbol

    -- * BeaconNames
  , genContractId
  , genOfferBeaconName
  , genAskBeaconName
  , genPremiumBeaconName
  , genTradingPairBeaconName

    -- * Creating Datums
  , NewProposalInfo(..)
  , unsafeCreateProposalDatum
  , NewActiveInfo(..)
  , unsafeCreateActiveDatum
  , createActiveDatumFromProposal
  , NewAddressInfo(..)
  , unsafeCreatePostAddressUpdateActiveDatum

    -- * Helper Functions
  , requiredAmount

    -- * Re-exports
  , module CardanoOptions.Types
  , module CardanoOptions.Utils
  ) where

import qualified PlutusTx
import qualified PlutusTx.Prelude as PlutusTx
import qualified Data.Map as Map
import Data.Aeson
import qualified Plutus.Script.Utils.Scripts as PV2
import qualified PlutusLedgerApi.V2 as PV2
import Relude
import Optics.TH
import Data.List ((!!))
import Optics.Getter (view)

import CardanoLoans (proxyScript,proxyValidatorHash)

import CardanoOptions.Blueprints
import CardanoOptions.Types
import CardanoOptions.Utils

-------------------------------------------------
-- On-Chain Datums
-------------------------------------------------
-- | The datum for an options contract that is available for purchase.
data ProposalDatum = ProposalDatum
  -- | The policy id for the proposal beacon script.
  { proposalBeaconId :: ProposalBeaconId
  -- | The policy id for the active beacon script.
  , activeBeaconId :: ActiveBeaconId
  -- | The asset being offered.
  , offerAsset :: OfferAsset
  -- | The amount of the offer asset being offered.
  , offerQuantity :: Integer
  -- | The asset being asked for.
  , askAsset :: AskAsset
  -- | The token name for the trading pair beacon.
  , tradingPairBeacon :: TradingPairBeacon
  -- | The token name for the offer beacon.
  , offerBeacon :: OfferBeacon
  -- | The token name for the ask beacon.
  , askBeacon :: AskBeacon
  -- | The asset the premium must be paid in.
  , premiumAsset :: PremiumAsset
  -- | The token name for the premium asset beacon.
  , premiumBeacon :: PremiumBeacon
  -- | The amount the writer paid for the minUTxOValue.
  , contractDeposit :: Integer
  -- | The address where the premium must go upon purchase of the contract.
  , paymentAddress :: Address
  -- | The possible terms the buyer can pick from.
  , possibleTerms :: [Terms]
  } deriving (Generic,Show)

PlutusTx.makeIsDataIndexed ''ProposalDatum [('ProposalDatum,0)]
makeFieldLabelsNoPrefix ''ProposalDatum

instance ToJSON ProposalDatum where
  toJSON ProposalDatum{..} =
    object [ "proposal_beacon_id" .= proposalBeaconId
           , "active_beacon_id" .= activeBeaconId
           , "offer_asset" .= offerAsset
           , "offer_quantity" .= offerQuantity
           , "ask_asset" .= askAsset
           , "trading_pair_beacon" .= tradingPairBeacon
           , "offer_beacon" .= offerBeacon
           , "ask_beacon" .= askBeacon
           , "premium_asset" .= premiumAsset
           , "premium_beacon" .= premiumBeacon
           , "contract_deposit" .= contractDeposit
           , "payment_address" .= paymentAddress
           , "possible_terms" .= possibleTerms
           ]

-- | The datum for an options contract that has been purchased, and can be executed any time up
-- until the expiration.
data ActiveDatum = ActiveDatum
  -- | The policy id for the proposal beacon script. This is needed to support using the same 
  -- active beacons redeemer for purchases, executions, and closing expired.
  { proposalBeaconId :: ProposalBeaconId
  -- | The policy id for the active beacon script.
  , activeBeaconId :: ActiveBeaconId
  -- | The hash of the address update observer script.
  , addressObserverHash :: ScriptHash
  -- | The asset being offered.
  , offerAsset :: OfferAsset
  -- | The amount of the offer asset being offered.
  , offerQuantity :: Integer
  -- | The asset being asked for.
  , askAsset :: AskAsset
  -- | The token name for the trading pair beacon.
  , tradingPairBeacon :: TradingPairBeacon
  -- | The token name for the offer beacon.
  , offerBeacon :: OfferBeacon
  -- | The token name for the ask beacon.
  , askBeacon :: AskBeacon
  -- | The strike price for this contract.
  , strikePrice :: Fraction
  -- | This contract's expiration.
  , expiration :: POSIXTime
  -- | The amount the writer paid for the minUTxOValue.
  , contractDeposit :: Integer
  -- | The address where the ask asset must go upon execution of the contract.
  , paymentAddress :: Address
  -- | The unique identitier for this contract.
  , contractId :: ContractId
  } deriving (Generic,Show)

instance ToJSON ActiveDatum where
  toJSON ActiveDatum{..} =
    object [ "proposal_beacon_id" .= proposalBeaconId
           , "active_beacon_id" .= activeBeaconId
           , "offer_asset" .= offerAsset
           , "offer_quantity" .= offerQuantity
           , "ask_asset" .= askAsset
           , "trading_pair_beacon" .= tradingPairBeacon
           , "offer_beacon" .= offerBeacon
           , "ask_beacon" .= askBeacon
           , "contract_deposit" .= contractDeposit
           , "payment_address" .= paymentAddress
           , "strike_price" .= strikePrice
           , "expiration" .= getPOSIXTime expiration
           , "contract_id" .= contractId
           ]

PlutusTx.makeIsDataIndexed ''ActiveDatum [('ActiveDatum,1)]
makeFieldLabelsNoPrefix ''ActiveDatum

newtype PaymentDatum = PaymentDatum (ActiveBeaconId,ContractId)
  deriving (Generic)

instance PV2.ToData PaymentDatum where
  toBuiltinData (PaymentDatum (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance PV2.FromData PaymentDatum where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,tok])) =
    fmap PaymentDatum . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData tok
  fromBuiltinData _ = Nothing

-------------------------------------------------
-- On-Chain Redeemers
-------------------------------------------------
data OptionsRedeemer
  -- | Close or update a Proposal UTxO.
  = CloseOrUpdateProposal
  -- | Purchase an options contract by converting a Proposal UTxO into an Active UTxO. The
  -- `desiredTermsIndex` identifies which `Terms` the buyer is purchasing.
  | PurchaseContract { desiredTermsIndex :: Integer }
  -- | Execute an active options contract.
  | ExecuteContract
  -- | Close an active options contract that has expired.
  | CloseExpiredContract
  -- | Update the address where the ask asset must go. Optionally deposit additional ada if needed.
  | UpdatePaymentAddress { newAddress :: Address, depositIncrease :: Integer }
  deriving (Generic,Show)

makeFieldLabelsNoPrefix ''OptionsRedeemer

data AddressObserverRedeemer
  -- | Observer a writer's address update transaction.
  = ObserveAddressUpdate
  -- | Register the script.
  | RegisterAddressObserverScript
  deriving (Generic,Show)

data ProposalBeaconsRedeemer
  -- | Create, close, or update some Proposal UTxOs (1 or more). 
  = CreateCloseOrUpdateProposals
  -- | Burn any beacons. 
  | BurnProposalBeacons
  -- | Register the script.
  | RegisterProposalScript
  deriving (Generic,Show)

data ActiveBeaconsRedeemer
  -- | Create some Active UTxOs (1 or more) by buying Proposal UTxOs. The CurrencySymbol is the 
  -- policy id for the proposal beacons.
  = PurchaseExecuteOrCloseExpiredContracts { proposalPolicyId :: CurrencySymbol }
  -- | Burn any beacons.
  | BurnActiveBeacons
  deriving (Generic,Show)

PlutusTx.unstableMakeIsData ''OptionsRedeemer
PlutusTx.unstableMakeIsData ''AddressObserverRedeemer
PlutusTx.unstableMakeIsData ''ProposalBeaconsRedeemer
PlutusTx.unstableMakeIsData ''ActiveBeaconsRedeemer

-------------------------------------------------
-- Contracts
-------------------------------------------------
optionsScript :: SerialisedScript
optionsScript = parseScriptFromCBOR $ blueprints Map.! "cardano_options.options_script"

optionsScriptHash :: PV2.ScriptHash
optionsScriptHash = scriptHash optionsScript

addressObserverScript :: SerialisedScript
addressObserverScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_options.address_observer_script")
    [ PV2.toData proxyValidatorHash
    , PV2.toData optionsScriptHash
    ]

addressObserverScriptHash :: PV2.ScriptHash
addressObserverScriptHash = scriptHash addressObserverScript

activeBeaconScript :: SerialisedScript
activeBeaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_options.active_beacon_script")
    [ PV2.toData optionsScriptHash
    , PV2.toData addressObserverScriptHash
    ]

activeBeaconScriptHash :: PV2.ScriptHash
activeBeaconScriptHash = scriptHash activeBeaconScript

activeBeaconCurrencySymbol :: PV2.CurrencySymbol
activeBeaconCurrencySymbol = PV2.CurrencySymbol $ PV2.getScriptHash $ activeBeaconScriptHash

proposalBeaconScript :: SerialisedScript
proposalBeaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_options.proposal_beacon_script")
    [ PV2.toData proxyValidatorHash
    , PV2.toData optionsScriptHash
    , PV2.toData activeBeaconScriptHash
    ]

proposalBeaconScriptHash :: PV2.ScriptHash
proposalBeaconScriptHash = scriptHash proposalBeaconScript

proposalBeaconCurrencySymbol :: PV2.CurrencySymbol
proposalBeaconCurrencySymbol = PV2.CurrencySymbol $ PV2.getScriptHash proposalBeaconScriptHash

-------------------------------------------------
-- Beacon Names
-------------------------------------------------
-- | Create the contract id from the proposal's output reference.
genContractId :: TxOutRef -> ContractId
genContractId (TxOutRef (TxId txHash) index) = 
  let TokenName index' = show index
  in ContractId $ TokenName $ PlutusTx.sha2_256 $ txHash <> index'

-- | Create the offer beacon name for the offer asset.
genOfferBeaconName :: OfferAsset -> OfferBeacon
genOfferBeaconName (OfferAsset ((CurrencySymbol sym),(TokenName name))) =
    OfferBeacon $ TokenName $ PlutusTx.sha2_256 $ prefix <> sym <> name
  where
    prefix :: BuiltinByteString
    prefix = unsafeToBuiltinByteString "01"

-- | Create the ask beacon name for the ask asset.
genAskBeaconName :: AskAsset -> AskBeacon
genAskBeaconName (AskAsset ((CurrencySymbol sym),(TokenName name))) =
    AskBeacon $ TokenName $ PlutusTx.sha2_256 $ prefix <> sym <> name
  where
    prefix :: BuiltinByteString
    prefix = unsafeToBuiltinByteString "02"

-- | Create the premium asset beacon name for the premium asset.
genPremiumBeaconName :: PremiumAsset -> PremiumBeacon
genPremiumBeaconName (PremiumAsset ((CurrencySymbol sym),(TokenName name))) =
    PremiumBeacon $ TokenName $ PlutusTx.sha2_256 $ prefix <> sym <> name
  where
    prefix :: BuiltinByteString
    prefix = unsafeToBuiltinByteString "03"

-- | Generate the beacon asset name by hashing offer ++ ask. The policy id for
-- ADA is set to "00".
genTradingPairBeaconName :: OfferAsset -> AskAsset -> TradingPairBeacon
genTradingPairBeaconName (OfferAsset assetX) (AskAsset assetY) =
    TradingPairBeacon $ TokenName $ PlutusTx.sha2_256 $ fullName assetX <> fullName assetY
  where
    fullName :: (CurrencySymbol,TokenName) -> BuiltinByteString
    fullName (CurrencySymbol sym, TokenName name)
      | sym == "" = unsafeToBuiltinByteString "00" <> name
      | otherwise = sym <> name

-------------------------------------------------
-- Creating datums
-------------------------------------------------
-- | Required information for creating a new ProposalDatum.
data NewProposalInfo = NewProposalInfo
  -- | The offer asset.
  { offerAsset :: OfferAsset
  -- | The amount of the offer asset being offered.
  , offerQuantity :: Integer
  -- | The asset being asked for.
  , askAsset :: AskAsset
  -- | The asset the premium must be paid in.
  , premiumAsset :: PremiumAsset
  -- | The amount the writer paid for the minUTxOValue.
  , contractDeposit :: Integer
  -- | The address where the premium must go upon purchase of the contract.
  , paymentAddress :: Address
  -- | The possible terms the buyer can pick from.
  , possibleTerms :: [Terms]
  } deriving (Show)

-- | Convert the proposal info to the ProposalDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreateProposalDatum :: NewProposalInfo -> ProposalDatum
unsafeCreateProposalDatum NewProposalInfo{..} = ProposalDatum
  { proposalBeaconId = ProposalBeaconId proposalBeaconCurrencySymbol
  , activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , offerAsset = offerAsset
  , offerQuantity = offerQuantity
  , askAsset = askAsset
  , tradingPairBeacon = genTradingPairBeaconName offerAsset askAsset
  , offerBeacon = genOfferBeaconName offerAsset
  , askBeacon = genAskBeaconName askAsset
  , premiumAsset = premiumAsset
  , premiumBeacon = genPremiumBeaconName premiumAsset
  , contractDeposit = contractDeposit
  , paymentAddress = paymentAddress
  , possibleTerms = possibleTerms
  }

-- | Required information for creating a new ActiveDatum.
data NewActiveInfo = NewActiveInfo
  -- | The offer asset.
  { offerAsset :: OfferAsset
  -- | The amount of the offer asset being offered.
  , offerQuantity :: Integer
  -- | The asset being asked for.
  , askAsset :: AskAsset
  -- | The strike price for this contract.
  , strikePrice :: Fraction
  -- | This contract's expiration.
  , expiration :: POSIXTime
  -- | The amount the writer paid for the minUTxOValue.
  , contractDeposit :: Integer
  -- | The address where the premium must go upon purchase of the contract.
  , paymentAddress :: Address
  -- | The associated proposal's output reference.
  , proposalId :: TxOutRef
  } deriving (Show)

-- | Convert the active info to the ActiveDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreateActiveDatum :: NewActiveInfo -> ActiveDatum
unsafeCreateActiveDatum NewActiveInfo{..} = ActiveDatum
  { proposalBeaconId = ProposalBeaconId proposalBeaconCurrencySymbol
  , activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , addressObserverHash = addressObserverScriptHash
  , offerAsset = offerAsset
  , offerQuantity = offerQuantity
  , askAsset = askAsset
  , tradingPairBeacon = genTradingPairBeaconName offerAsset askAsset
  , offerBeacon = genOfferBeaconName offerAsset
  , askBeacon = genAskBeaconName askAsset
  , strikePrice = strikePrice
  , expiration = expiration
  , contractDeposit = contractDeposit
  , paymentAddress = paymentAddress
  , contractId = genContractId proposalId
  }

-- | Create an ActiveDatum from a ProposalDatum, its output reference, and the desiredTermsIndex.
createActiveDatumFromProposal :: Int -> TxOutRef -> ProposalDatum -> ActiveDatum
createActiveDatumFromProposal termsIndex proposalId ProposalDatum{..} = ActiveDatum
    { proposalBeaconId = proposalBeaconId
    , activeBeaconId = activeBeaconId
    , addressObserverHash = addressObserverScriptHash
    , offerAsset = offerAsset
    , offerQuantity = offerQuantity
    , askAsset = askAsset
    , tradingPairBeacon = tradingPairBeacon
    , offerBeacon = offerBeacon
    , askBeacon = askBeacon
    , strikePrice = view #strikePrice desiredTerms
    , expiration = view #expiration desiredTerms
    , contractDeposit = contractDeposit
    , paymentAddress = paymentAddress
    , contractId = genContractId proposalId
    }
  where
    desiredTerms :: Terms
    desiredTerms = possibleTerms!!termsIndex

-- | Required information for creating an ActiveDatum with a new payment address.
data NewAddressInfo = NewAddressInfo
  -- | The offer asset.
  { offerAsset :: OfferAsset
  -- | The amount of the offer asset being offered.
  , offerQuantity :: Integer
  -- | The asset being asked for.
  , askAsset :: AskAsset
  -- | The strike price for this contract.
  , strikePrice :: Fraction
  -- | This contract's expiration.
  , expiration :: POSIXTime
  -- | The amount the writer paid for the minUTxOValue.
  , contractDeposit :: Integer
  -- | The address where the premium must go upon purchase of the contract.
  , paymentAddress :: Address
  -- | The associated proposal's output reference.
  , contractId :: ContractId
  } deriving (Show)

-- | Convert the address info to the ActiveDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreatePostAddressUpdateActiveDatum :: NewAddressInfo -> ActiveDatum
unsafeCreatePostAddressUpdateActiveDatum NewAddressInfo{..} = ActiveDatum
  { proposalBeaconId = ProposalBeaconId proposalBeaconCurrencySymbol
  , activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , addressObserverHash = addressObserverScriptHash
  , offerAsset = offerAsset
  , offerQuantity = offerQuantity
  , askAsset = askAsset
  , tradingPairBeacon = genTradingPairBeaconName offerAsset askAsset
  , offerBeacon = genOfferBeaconName offerAsset
  , askBeacon = genAskBeaconName askAsset
  , strikePrice = strikePrice
  , expiration = expiration
  , contractDeposit = contractDeposit
  , paymentAddress = paymentAddress
  , contractId = contractId
  }

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | The total amount of the Ask asset required based on the offer quantity and strike price.
requiredAmount :: Integer -> Fraction -> Integer
requiredAmount offerQuantity (Fraction (num,den)) = (offerQuantity * num) `div` den

