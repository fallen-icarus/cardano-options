module CLI.Run
(
  runCommand
) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Text.Encoding as TE
import qualified Codec.Binary.Encoding as E

import CLI.Types
import CardanoOptions

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ExportScript script file -> runExportScriptCmd script file
  CreateOptionsDatum d file -> writeData file d

runExportScriptCmd :: Script -> FilePath -> IO ()
runExportScriptCmd script file = do
  let script' = case script of
        OptionsScript -> optionsValidatorScript
        BeaconPolicy config -> optionsBeaconPolicyScript config
  res <- writeScript file script'
  case res of
    Right _ -> return ()
    Left err -> putStrLn $ "There was an error: " <> show err