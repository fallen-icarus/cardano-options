{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module CardanoOptions.Types
  ( ProposalBeaconId(..)
  , ActiveBeaconId(..)
  , Fraction(..)
  , OfferAsset(..)
  , AskAsset(..)
  , PremiumAsset(..)
  , ContractId(..)
  , TradingPairBeacon(..)
  , OfferBeacon(..)
  , AskBeacon(..)
  , PremiumBeacon(..)
  , Terms(..)
  ) where

import GHC.Generics
import Data.Aeson
import Data.Text qualified as T
import Prettyprinter
import Optics.TH

import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusTx

-------------------------------------------------
-- Helpers
-------------------------------------------------
unsafeFromData :: (PV2.UnsafeFromData a) => PV2.Data -> a
unsafeFromData = PV2.unsafeFromBuiltinData . PV2.dataToBuiltinData

-------------------------------------------------
-- ProposalBeaconId
-------------------------------------------------
-- | A wrapper around the policy id for the proposal beacon script.
newtype ProposalBeaconId = ProposalBeaconId { unProposalBeaconId :: PV2.CurrencySymbol }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON ProposalBeaconId where
  toJSON (ProposalBeaconId currSym) = toJSON $ T.pack $ show currSym

makeFieldLabelsNoPrefix ''ProposalBeaconId

-------------------------------------------------
-- ActiveBeaconId
-------------------------------------------------
-- | A wrapper around the policy id for the active beacon script.
newtype ActiveBeaconId = ActiveBeaconId { unActiveBeaconId :: PV2.CurrencySymbol }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON ActiveBeaconId where
  toJSON (ActiveBeaconId currSym) = toJSON $ T.pack $ show currSym

makeFieldLabelsNoPrefix ''ActiveBeaconId

-------------------------------------------------
-- Fraction
-------------------------------------------------
-- | A wrapper around two integers that make up a fraction. This is used
-- in the absence of a decimal type on change.
newtype Fraction = Fraction { unFraction :: (Integer,Integer) }
  deriving (Show,Eq)

instance PV2.ToData Fraction where
  toBuiltinData (Fraction (num,den)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData num, PV2.toData den]

instance PV2.FromData Fraction where
  fromBuiltinData (PV2.BuiltinData (PV2.List [num,den])) =
    fmap Fraction . (,) 
      <$> PV2.fromData num 
      <*> PV2.fromData den
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData Fraction where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [num,den])) = 
    Fraction (unsafeFromData num, unsafeFromData den)
  unsafeFromBuiltinData _ = error "Could not convert Data to Fraction"

instance ToJSON Fraction where
  toJSON (Fraction (num,den)) =
    object [ "numerator" .= T.pack (show num)
           , "denominator" .= T.pack (show den)
           ]

instance Pretty Fraction where
  pretty (Fraction (num,den)) = 
    pretty num <+> "/" <+> pretty den <+> 
      tupled [pretty @Double (fromIntegral num / fromIntegral den)]

makeFieldLabelsNoPrefix ''Fraction

-------------------------------------------------
-- OfferAsset
-------------------------------------------------
-- | A wrapper around the offer asset's full name (policy id, token name). It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype OfferAsset = OfferAsset { unOfferAsset :: (PV2.CurrencySymbol,PV2.TokenName) }
  deriving (Show,Eq)

instance PV2.ToData OfferAsset where
  toBuiltinData (OfferAsset (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance PV2.FromData OfferAsset where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) =
    fmap OfferAsset . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData name
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData OfferAsset where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) = 
    OfferAsset (unsafeFromData sym, unsafeFromData name)
  unsafeFromBuiltinData _ = error "Could not convert Data to OfferAsset"

instance ToJSON OfferAsset where
  toJSON (OfferAsset (currSym,PV2.TokenName tokName)) =
    object [ "policy_id" .= T.pack (show currSym)
           , "asset_name" .= T.pack (show $ PV2.PubKeyHash tokName)
           ]

instance Pretty OfferAsset where
  pretty (OfferAsset (currSym,PV2.TokenName tokName)) = 
    if currSym == "" 
    then "lovelace"
    else pretty $ T.pack (show currSym) <> "." <> T.pack (show $ PV2.PubKeyHash tokName)

makeFieldLabelsNoPrefix ''OfferAsset

-------------------------------------------------
-- AskAsset
-------------------------------------------------
-- | A wrapper around the ask asset's full name (policy id, token name). It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype AskAsset = AskAsset { unAskAsset :: (PV2.CurrencySymbol,PV2.TokenName) }
  deriving (Show,Eq)

instance PV2.ToData AskAsset where
  toBuiltinData (AskAsset (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance PV2.FromData AskAsset where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) =
    fmap AskAsset . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData name
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData AskAsset where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) = 
    AskAsset (unsafeFromData sym, unsafeFromData name)
  unsafeFromBuiltinData _ = error "Could not convert Data to AskAsset"

instance ToJSON AskAsset where
  toJSON (AskAsset (currSym,PV2.TokenName tokName)) =
    object [ "policy_id" .= T.pack (show currSym)
           , "asset_name" .= T.pack (show $ PV2.PubKeyHash tokName)
           ]

instance Pretty AskAsset where
  pretty (AskAsset (currSym,PV2.TokenName tokName)) = 
    if currSym == "" 
    then "lovelace"
    else pretty $ T.pack (show currSym) <> "." <> T.pack (show $ PV2.PubKeyHash tokName)

makeFieldLabelsNoPrefix ''AskAsset

-------------------------------------------------
-- PremiumAsset
-------------------------------------------------
-- | A wrapper around the premium asset's full name (policy id, token name). It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype PremiumAsset = PremiumAsset { unPremiumAsset :: (PV2.CurrencySymbol,PV2.TokenName) }
  deriving (Show,Eq)

instance PV2.ToData PremiumAsset where
  toBuiltinData (PremiumAsset (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance PV2.FromData PremiumAsset where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) =
    fmap PremiumAsset . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData name
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData PremiumAsset where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) = 
    PremiumAsset (unsafeFromData sym, unsafeFromData name)
  unsafeFromBuiltinData _ = error "Could not convert Data to PremiumAsset"

instance ToJSON PremiumAsset where
  toJSON (PremiumAsset (currSym,PV2.TokenName tokName)) =
    object [ "policy_id" .= T.pack (show currSym)
           , "asset_name" .= T.pack (show $ PV2.PubKeyHash tokName)
           ]

instance Pretty PremiumAsset where
  pretty (PremiumAsset (currSym,PV2.TokenName tokName)) = 
    if currSym == "" 
    then "lovelace"
    else pretty $ T.pack (show currSym) <> "." <> T.pack (show $ PV2.PubKeyHash tokName)

makeFieldLabelsNoPrefix ''PremiumAsset

-------------------------------------------------
-- ContractId
-------------------------------------------------
-- | A wrapper around the token name for a contract's unique identifier.
newtype ContractId = ContractId { unContractId :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON ContractId where
  toJSON (ContractId (PV2.TokenName tokName)) = toJSON $ T.pack $ show $ PV2.PubKeyHash tokName 

instance Pretty ContractId where
  pretty (ContractId (PV2.TokenName tokName)) = pretty $ T.pack $ show $ PV2.PubKeyHash tokName 

makeFieldLabelsNoPrefix ''ContractId

-------------------------------------------------
-- TradingPairBeacon
-------------------------------------------------
-- | A wrapper around the token name for the beacon associated with that trading pair.
newtype TradingPairBeacon = TradingPairBeacon { unTradingPairBeacon :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON TradingPairBeacon where
  toJSON (TradingPairBeacon (PV2.TokenName tokName)) = toJSON $ T.pack $ show $ PV2.PubKeyHash tokName 

instance Pretty TradingPairBeacon where
  pretty (TradingPairBeacon (PV2.TokenName tokName)) = pretty $ T.pack $ show $ PV2.PubKeyHash tokName 

makeFieldLabelsNoPrefix ''TradingPairBeacon

-------------------------------------------------
-- OfferBeacon
-------------------------------------------------
-- | A wrapper around the token name for the beacon associated with that offer asset.
newtype OfferBeacon = OfferBeacon { unOfferBeacon :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON OfferBeacon where
  toJSON (OfferBeacon (PV2.TokenName tokName)) = toJSON $ T.pack $ show $ PV2.PubKeyHash tokName 

instance Pretty OfferBeacon where
  pretty (OfferBeacon (PV2.TokenName tokName)) = pretty $ T.pack $ show $ PV2.PubKeyHash tokName 

makeFieldLabelsNoPrefix ''OfferBeacon

-------------------------------------------------
-- AskBeacon
-------------------------------------------------
-- | A wrapper around the token name for the beacon associated with that ask asset.
newtype AskBeacon = AskBeacon { unAskBeacon :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON AskBeacon where
  toJSON (AskBeacon (PV2.TokenName tokName)) = toJSON $ T.pack $ show $ PV2.PubKeyHash tokName 

instance Pretty AskBeacon where
  pretty (AskBeacon (PV2.TokenName tokName)) = pretty $ T.pack $ show $ PV2.PubKeyHash tokName 

makeFieldLabelsNoPrefix ''AskBeacon

-------------------------------------------------
-- PremiumBeacon
-------------------------------------------------
-- | A wrapper around the token name for the beacon associated with that premium asset.
newtype PremiumBeacon = PremiumBeacon { unPremiumBeacon :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON PremiumBeacon where
  toJSON (PremiumBeacon (PV2.TokenName tokName)) = toJSON $ T.pack $ show $ PV2.PubKeyHash tokName 

instance Pretty PremiumBeacon where
  pretty (PremiumBeacon (PV2.TokenName tokName)) = pretty $ T.pack $ show $ PV2.PubKeyHash tokName 

makeFieldLabelsNoPrefix ''PremiumBeacon

-------------------------------------------------
-- Terms
-------------------------------------------------
-- | The terms that an options' writer can vary within the same Proposal UTxO.
data Terms = Terms
  { premium :: Integer
  , strikePrice :: Fraction
  , expiration :: PV2.POSIXTime
  } deriving (Generic,Show,Eq)

instance ToJSON Terms where
  toJSON Terms{..} =
    object [ "premium" .= premium
           , "strike_price" .= strikePrice
           , "expiration" .= PV2.getPOSIXTime expiration
           ]
makeFieldLabelsNoPrefix ''Terms
PlutusTx.unstableMakeIsData ''Terms
