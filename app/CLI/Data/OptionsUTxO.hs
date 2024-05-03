{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module CLI.Data.OptionsUTxO where

import Relude
import Data.Aeson
import Prettyprinter
import Prettyprinter.Render.Terminal

import CLI.Data.Asset
import CLI.Data.Bech32Address
import CLI.Data.Network

import CardanoOptions

data OptionsDatum
  = Proposal ProposalDatum
  | Active ActiveDatum
  deriving (Show)

instance ToJSON OptionsDatum where
  toJSON (Proposal datum) =
    object [ "type" .= ("proposal" :: Text)
           , "datum" .= datum
           ]
  toJSON (Active datum) =
    object [ "type" .= ("active" :: Text)
           , "datum" .= datum
           ]

data OptionsUTxO = OptionsUTxO
  { optionsAddress :: PaymentAddress
  , utxoRef :: TxOutRef
  , lovelaces :: Lovelace
  , nativeAssets :: [NativeAsset]
  , optionsDatum :: Maybe OptionsDatum
  } deriving (Show)

instance FromJSON OptionsUTxO where
  parseJSON =
      withObject "OptionsUTxO" $ \o ->
        OptionsUTxO
          <$> o .: "address"
          <*> ( (concatRef <$> o .: "tx_hash" <*> o .: "tx_index") >>= 
                  maybe mzero return . rightToMaybe . readTxOutRef)
          <*> o .: "value"
          <*> o .: "asset_list"
          <*> (o .:? "inline_datum" >>= 
                maybe (return Nothing) (\i -> withObject "inlineDatum" (.: "value") i >>= return . parseDatum))
    where
      concatRef :: String -> Integer -> String
      concatRef hash idx = hash <> "#" <> show idx

      parseDatum :: Value -> Maybe OptionsDatum
      parseDatum v = (Proposal <$> decodeDatum @ProposalDatum v)
                 <|> (Active <$> decodeDatum @ActiveDatum v)

instance ToJSON OptionsUTxO where
  toJSON OptionsUTxO{..} =
    object [ "options_address" .= optionsAddress
           , "utxo_id" .= (\(TxOutRef hash idx) -> show @Text hash <> "#" <> show idx) utxoRef
           , "native_assets" .= nativeAssets
           , "lovelace" .= unLovelace lovelaces
           , "options_info" .= optionsDatum
           ]

prettyOptionsUTxO :: Network -> OptionsUTxO -> Doc AnsiStyle
prettyOptionsUTxO network OptionsUTxO{utxoRef=(TxOutRef hash idx),..} = 
  vsep [ annotate (colorDull Yellow) "utxo_ref:" <+> show hash <> "#" <> show idx
       , indent 4 $ annotate (colorDull Green) "options_address:" <+> pretty optionsAddress 
       , indent 4 $ annotate (colorDull Green) "assets:"
       , indent 6 $ pretty lovelaces <+> "lovelace"
       , indent 6 $ align $ vsep $ map pretty nativeAssets
       , indent 4 $ 
           maybe (annotate (colorDull Cyan) "datum:" <+> "none") prettyOptionsDatum optionsDatum
       , mempty
       ]
  where
    config = case network of
      Mainnet -> mainnetConfig
      PreProdTestnet -> preprodConfig

    prettyTerms :: [Terms] -> Int -> Doc AnsiStyle
    prettyTerms [] _ = mempty
    prettyTerms (Terms{..}:xs) i =
      let time = getPOSIXTime expiration in
        vsep 
          [ annotate (colorDull Cyan) ("choice_" <> pretty i <> ":")
          , indent 2 $ align $
              vsep
                [ annotate (colorDull Cyan) "premium:" <+> pretty premium
                , annotate (colorDull Cyan) "strike_price:" <+> pretty strikePrice
                , annotate (colorDull Cyan) "expiration:" 
                    <+> pretty (posixTimeToSlot config expiration)
                    <+> tupled [pretty time <+> "posix"]
                ]
          , mempty
          ] <> prettyTerms xs (i+1)

    prettyOptionsDatum :: OptionsDatum -> Doc AnsiStyle
    prettyOptionsDatum (Proposal ProposalDatum{..}) =
      vsep [ annotate (colorDull Cyan) "type:" <+> pretty @Text "Proposal"
           , annotate (colorDull Cyan) "offer:" <+> pretty offerQuantity <+> pretty offerAsset
           , annotate (colorDull Cyan) "ask_asset:" <+> pretty askAsset
           , annotate (colorDull Cyan) "premium_asset:" <+> pretty premiumAsset
           , annotate (colorDull Cyan) "contract_deposit:" <+> pretty (Lovelace contractDeposit)
           , annotate (colorDull Cyan) "payment_address:" <+> 
               pretty (either (const "failed to convert to bech32") fst $ 
                 plutusToBech32 network paymentAddress)
           , prettyTerms possibleTerms 0
           ]
    prettyOptionsDatum (Active ActiveDatum{..}) =
      let time = getPOSIXTime expiration in
        vsep [ annotate (colorDull Cyan) "type:" <+> pretty @Text "Active"
             , annotate (colorDull Cyan) "offer:" <+> pretty offerQuantity <+> pretty offerAsset
             , annotate (colorDull Cyan) "ask_asset:" <+> pretty askAsset
             , annotate (colorDull Cyan) "contract_deposit:" <+> pretty (Lovelace contractDeposit)
             , annotate (colorDull Cyan) "payment_address:" <+> 
                 pretty (either (const "failed to convert to bech32") fst $ 
                   plutusToBech32 network paymentAddress)
             , annotate (colorDull Cyan) "strike_price:" <+> pretty strikePrice
             , annotate (colorDull Cyan) "expiration:" 
                  <+> pretty (posixTimeToSlot config expiration)
                  <+> tupled [pretty time <+> "posix"]
             , annotate (colorDull Cyan) "contract_id:" <+> pretty contractId
             ]
