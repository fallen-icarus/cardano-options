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

data Convert
  = POSIXTimeToSlot POSIXTime
  | SlotToPOSIXTime Slot

data Script = BeaconPolicy OptionsConfig | OptionsScript

-- | For when saving to file is optional
data Output = Stdout | File FilePath

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
