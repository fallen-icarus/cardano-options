{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Types where

import Data.Text
import GHC.Word (Word32)
import Data.Aeson

import CardanoOptions

data Command
  = ExportScript Script FilePath
  | CreateOptionsDatum OptionsDatum FilePath
  | CreateOptionsRedeemer OptionsRedeemer FilePath
  | CreateBeaconRedeemer OptionsBeaconRedeemer FilePath
  | ExtractAddressHashes Text Output
  | GenerateBech32Address Address Output
  | Convert Convert
  | QueryBeacons Query

data Query
  = QueryAvailableContracts Network CurrencySymbol Output
  | QueryOwnAssetsUTxOs Network CurrencySymbol OptionsAddress Output
  | QueryOwnProposedUTxOs Network CurrencySymbol OptionsAddress Output
  | QueryOwnActiveUTxOs Network CurrencySymbol OptionsAddress Output
  | QuerySpecificContract Network CurrencySymbol TokenName Output

data Convert
  = POSIXTimeToSlot POSIXTime
  | SlotToPOSIXTime Slot

data Script = BeaconPolicy OptionsConfig | OptionsScript

-- | For when saving to file is optional
data Output = Stdout | File FilePath

newtype OptionsAddress = OptionsAddress String

instance Show OptionsAddress where
  show (OptionsAddress s) = s

data Network 
  = PreProdTestnet String  -- ^ Api key

data AddressInfo' = AddressInfo'
  { addressSpendingKeyHash :: Maybe Text
  , addressSpendingScriptHash :: Maybe Text
  , addressStakeKeyHash :: Maybe Text
  , addressStakeScriptHash :: Maybe Text
  , addressNetworkTag :: Word32
  }

instance ToJSON AddressInfo' where
  toJSON AddressInfo'{..} =
    object [ "payment_pubkey_hash" .= addressSpendingKeyHash 
           , "payment_script_hash" .= addressSpendingScriptHash
           , "staking_pubkey_hash" .= addressStakeKeyHash
           , "staking_script_hash" .= addressStakeScriptHash
           , "network_tag" .= addressNetworkTag
           ]

data Asset = Asset
  { assetPolicyId :: String
  , assetTokenName :: String
  , assetQuantity :: Integer
  } deriving (Show)

instance ToJSON Asset where
  toJSON Asset{..} =
    object [ "asset" .= if assetPolicyId == "lovelace" 
                        then "lovelace" 
                        else assetPolicyId <> "." <> assetTokenName
           , "quantity" .= assetQuantity
           ]

data UTxOType = Assets | Proposal | Active deriving (Show)

data UTxOInfo = UTxOInfo
  { txHash :: String
  , outputIndex :: String
  , utxoValue :: [Asset]
  , optionInfo :: OptionsDatum
  }

instance ToJSON UTxOInfo where
  toJSON UTxOInfo{..} =
    object [ "tx_hash" .= txHash
           , "output_index" .= outputIndex
           , "utxo_assets" .= utxoValue
           , "info" .= optionInfo
           ]

data AvailableContractInfo = AvailableContractInfo
  { address :: String
  , assetsUTxO :: UTxOInfo
  , proposedUTxOs :: [UTxOInfo]
  }

instance ToJSON AvailableContractInfo where
  toJSON AvailableContractInfo{..} =
    object [ "address" .= address
           , "assets_utxo" .= assetsUTxO
           , "proposed_utxos" .= proposedUTxOs
           ]