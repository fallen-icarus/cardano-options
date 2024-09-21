{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module CLI.Data.PersonalUTxO where

import Relude
import Data.Aeson
import Prettyprinter
import Prettyprinter.Render.Terminal

import CLI.Data.Bech32Address
import CLI.Data.Asset

import CardanoOptions hiding (datumHash)

data PersonalUTxO = PersonalUTxO
  { paymentAddress :: PaymentAddress
  , utxoRef :: TxOutRef
  , lovelaces :: Lovelace
  , datumHash :: Maybe Text
  , referenceScriptHash :: Maybe Text
  , nativeAssets :: [NativeAsset]
  } deriving (Show,Eq)

instance Ord PersonalUTxO where
  PersonalUTxO{utxoRef=ref1} <= PersonalUTxO{utxoRef=ref2} = ref1 <= ref2

instance FromJSON PersonalUTxO where
  parseJSON =
      withObject "PersonalUTxO" $ \o ->
        PersonalUTxO
          <$> o .: "address"
          <*> ( (concatRef <$> o .: "tx_hash" <*> o .: "tx_index") >>= 
                  maybe mzero return . rightToMaybe . readTxOutRef)
          <*> o .: "value"
          <*> o .: "datum_hash"
          <*> (o .: "reference_script" >>= 
                maybe (return Nothing) (withObject "referenceScript" $ \i -> i .: "hash"))
          <*> o .: "asset_list"
    where
      concatRef :: String -> Integer -> String
      concatRef hash idx = hash <> "#" <> show idx

instance ToJSON PersonalUTxO where
  toJSON PersonalUTxO{..} =
    object [ "address" .= paymentAddress
           , "utxo_id" .= (\(TxOutRef hash idx) -> show @Text hash <> "#" <> show idx) utxoRef
           , "reference_script_hash" .= referenceScriptHash
           , "datum_hash" .= datumHash
           , "native_assets" .= nativeAssets
           , "lovelace" .= unLovelace lovelaces
           ]

prettyPersonalUTxO :: PersonalUTxO -> Doc AnsiStyle
prettyPersonalUTxO PersonalUTxO{utxoRef=(TxOutRef txHash idx),..} =
  hsep $ mconcat
    [ [ pretty txHash 
      , "   "
      , pretty idx
      , "      "
      , pretty (unLovelace lovelaces) <+> "lovelace"
      ]
    , map pretty nativeAssets
    , mDatum
    , mScriptHash
    ]
  where
    mDatum :: [Doc AnsiStyle]
    mDatum = flip (maybe []) datumHash $ \hash ->
      ["+" <+> annotate (color Green) ("DatumHash" <+> pretty hash)]

    mScriptHash :: [Doc AnsiStyle]
    mScriptHash = flip (maybe []) referenceScriptHash $ \hash ->
      ["+" <+> annotate (color Blue) ("ScriptHash" <+> pretty hash)]

personalHeader :: Doc ann
personalHeader = 
  let s = "                           TxHash                                 TxIx        Amount"
  in mconcat
      [ pretty @String s
      , hardline
      , pretty $ replicate (length s + 2) '-'
      , hardline
      ]
