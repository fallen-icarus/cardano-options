{-# LANGUAGE StrictData #-}

module CLI.Types where

import CardanoOptions

data Command
  = ExportScript Script FilePath
  | CreateOptionsDatum OptionsDatum FilePath

data Script = BeaconPolicy OptionsConfig | OptionsScript