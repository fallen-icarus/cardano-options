{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Query.Koios
  ( queryPersonalAddress
  , querySlotTip
  , queryOptionsUTxOs
  , querySpecificOptionsUTxO
  , submitTx
  , evaluateTx
  ) where

import Relude
import Servant.API
import Data.Aeson
import Servant.Client
import qualified Data.Text as T

import CardanoOptions

import CLI.Data.Bech32Address
import CLI.Data.OptionsUTxO
import CLI.Data.PersonalUTxO
import CLI.Data.TxCBOR

-------------------------------------------------
-- Post Types
-------------------------------------------------
instance ToHttpApiData CurrencySymbol where
  toQueryParam = show

instance ToHttpApiData TokenName where
  toQueryParam = T.pack . showTokenName

-- | A newtype for submitting a list of payment addresses with the "_extended" flag.
newtype ExtendedPaymentAddresses = ExtendedPaymentAddresses [PaymentAddress] 
  deriving (Show)

instance ToJSON ExtendedPaymentAddresses where
  toJSON (ExtendedPaymentAddresses as) = 
    object [ "_addresses" .= map unPaymentAddress as 
           , "_extended" .= True
           ]

newtype TargetAsset = TargetAsset (CurrencySymbol,TokenName)

instance ToJSON TargetAsset where
  toJSON (TargetAsset (currSym,tokName)) = object 
    [ "_asset_list" .= [[T.pack $ show currSym, T.pack $ showTokenName tokName]]
    , "_extended" .= True
    ]

newtype ExtendedUTxOList = ExtendedUTxOList { unUTxOList :: [TxOutRef] } deriving (Show)

instance ToJSON ExtendedUTxOList where
  toJSON (ExtendedUTxOList as) = 
    object [ "_utxo_refs" .= map (\(TxOutRef hash ix) -> T.pack $ show hash <> "#" <> show ix) as
           , "_extended" .= True
           ]

newtype SubmitTxCBOR = SubmitTxCBOR TxCBOR

instance ToJSON SubmitTxCBOR where
  toJSON (SubmitTxCBOR (TxCBOR cbor)) = 
    object [ "jsonrpc" .= ("2.0" :: Text)
           , "method" .= ("submitTransaction" :: Text)
           , "params" .= object [ "transaction" .= object [ "cbor" .= cbor ] ]
           , "id" .= (Nothing :: Maybe ())
           ]

newtype EvaluateTxCBOR = EvaluateTxCBOR TxCBOR

instance ToJSON EvaluateTxCBOR where
  toJSON (EvaluateTxCBOR (TxCBOR cbor)) = 
    object [ "jsonrpc" .= ("2.0" :: Text)
           , "method" .= ("evaluateTransaction" :: Text)
           , "params" .= object [ "transaction" .= object [ "cbor" .= cbor ] ]
           , "id" .= (Nothing :: Maybe ())
           ]

-------------------------------------------------
-- Intermediate Response Types
-------------------------------------------------
newtype SlotTip = SlotTip { _unSlotTip :: Integer }
  deriving (Show)

instance FromJSON SlotTip where
  parseJSON (Object o) = SlotTip <$> o .: "abs_slot"
  parseJSON _ = mzero

-------------------------------------------------
-- Low-Level API
-------------------------------------------------
type KoiosApi
  =     "address_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] ExtendedPaymentAddresses
     :> Post '[JSON] [PersonalUTxO]

  :<|>  "address_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] ExtendedPaymentAddresses
     :> Post '[JSON] [OptionsUTxO]

  :<|>  "asset_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] TargetAsset
     :> Post '[JSON] [Maybe OptionsUTxO]

  :<|>  "utxo_info"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] ExtendedUTxOList
     :> Post '[JSON] [OptionsUTxO]

  :<|>  "tip"
     :> Get '[JSON] [SlotTip]

  :<|>  ReqBody '[JSON] SubmitTxCBOR
     :> Post '[JSON] Value

  :<|>  ReqBody '[JSON] EvaluateTxCBOR
     :> Post '[JSON] Value

personalAddressUTxOsApi 
  :<|> writerAddressUTxOsApi
  :<|> optionsUTxOsApi
  :<|> specificOptionsUTxOsApi
  :<|> slotTipApi
  :<|> submitTxApi
  :<|> evaluateTxApi
  = client (Proxy :: Proxy KoiosApi)

-------------------------------------------------
-- High-Level API
-------------------------------------------------
queryPersonalAddress :: PaymentAddress -> Bool -> ClientM [PersonalUTxO]
queryPersonalAddress addr keysOnly =
    sortOn (\PersonalUTxO{utxoRef} -> utxoRef) <$>
      personalAddressUTxOsApi select "eq.false" keyFilter (ExtendedPaymentAddresses [addr])
  where
    keyFilter
      | keysOnly = Just keysQueryParam
      | otherwise = Nothing
    select =
      toText $ intercalate ","
        [ "is_spent"
        , "tx_hash"
        , "tx_index"
        , "address"
        , "value"
        , "datum_hash"
        , "asset_list"
        , "reference_script"
        ]

querySlotTip :: ClientM Integer
querySlotTip = slotTipApi >>= \case
  [(SlotTip t)] -> return t
  _ -> error "slotTipApi error"

queryOptionsUTxOs :: [(CurrencySymbol,TokenName)] -> Maybe PaymentAddress -> ClientM [OptionsUTxO]
queryOptionsUTxOs [] Nothing = return []
queryOptionsUTxOs [] (Just addr) = do
    writerAddressUTxOsApi select "eq.false" Nothing (ExtendedPaymentAddresses [addr])  
  where
    select :: Text
    select =
      toText $ intercalate ","
        [ "is_spent"
        , "tx_hash"
        , "tx_index"
        , "address"
        , "value"
        , "inline_datum"
        , "asset_list"
        ]
queryOptionsUTxOs targetBeacons@(x:xs) mWriterAddr = case mWriterAddr of
    Nothing -> do
      let assetFilter = Just $ assetToQueryParam xs
      catMaybes <$> optionsUTxOsApi select "eq.false" assetFilter (TargetAsset x)
    Just addr -> do
      let assetFilter = Just $ assetToQueryParam targetBeacons
      writerAddressUTxOsApi select "eq.false" assetFilter (ExtendedPaymentAddresses [addr])  
  where
    select :: Text
    select =
      toText $ intercalate ","
        [ "is_spent"
        , "tx_hash"
        , "tx_index"
        , "address"
        , "value"
        , "inline_datum"
        , "asset_list"
        ]

querySpecificOptionsUTxO :: TxOutRef -> ClientM [OptionsUTxO]
querySpecificOptionsUTxO outRef =
    specificOptionsUTxOsApi select "eq.false" Nothing (ExtendedUTxOList [outRef])  
  where
    select =
      toText $ intercalate ","
        [ "is_spent"
        , "tx_hash"
        , "tx_index"
        , "address"
        , "value"
        , "inline_datum"
        , "asset_list"
        ]

submitTx :: TxCBOR -> ClientM Value
submitTx = submitTxApi . SubmitTxCBOR

evaluateTx :: TxCBOR -> ClientM Value
evaluateTx = evaluateTxApi . EvaluateTxCBOR

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
assetToQueryParam :: [(CurrencySymbol,TokenName)] -> Text
assetToQueryParam assets = "cs.[" <> T.intercalate "," (go assets) <> "]"
  where
    go [] = []
    go ((currSym,tokName):xs) = 
      let policyId = T.pack $ show currSym
          assetName = T.pack $ showTokenName tokName
       in ("{\"policy_id\":\"" <> policyId <> "\",\"asset_name\":\"" <> assetName <> "\"}") : go xs

keysQueryParam :: Text
keysQueryParam = "cs.[{\"policy_id\":\"" <> show activeBeaconCurrencySymbol <> "\"}]"
