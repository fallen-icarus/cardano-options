{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
import CLI.Query
import CardanoOptions
import Cardano.Address.Style.Shelley hiding (unsafeFromRight)
import Cardano.Address (fromBech32,unNetworkTag,bech32)
import Cardano.Address.Script hiding (Script)

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ExportScript script file -> runExportScriptCmd script file
  CreateOptionsDatum d file -> writeData file d
  CreateOptionsRedeemer r file -> writeData file r
  CreateBeaconRedeemer r file -> writeData file r
  ConvertAddress convert output -> runAddressConversion convert output
  ConvertTime convert -> runTimeConversion convert
  QueryBeacons query -> runQuery query

runExportScriptCmd :: Script -> FilePath -> IO ()
runExportScriptCmd script file = do
  let script' = case script of
        OptionsScript -> optionsValidatorScript
        BeaconPolicy config -> optionsBeaconPolicyScript config
  res <- writeScript file script'
  case res of
    Right _ -> return ()
    Left err -> putStrLn $ "There was an error: " <> show err

runAddressConversion :: ConvertAddress -> Output -> IO ()
runAddressConversion (Plutus addr) output = generateBech32Address addr output
runAddressConversion (Bech32 addr) output = extractAddressInfo addr output

runTimeConversion :: ConvertTime -> IO ()
runTimeConversion (POSIXTimeToSlot p) = print $ getSlot $ posixTimeToSlot p
runTimeConversion (SlotToPOSIXTime s) = print $ getPOSIXTime $ slotToPOSIXTime s

runQuery :: Query -> IO ()
runQuery query = case query of
  QueryAvailableContracts network policyId output -> 
    runQueryAvailableContracts network policyId >>= toOutput output
  QueryOwnAssetsUTxOs network policyId addr output ->
    runQueryOwnAssets network policyId addr >>= toOutput output
  QueryOwnProposedUTxOs network policyId addr output ->
    runQueryOwnProposals network policyId addr >>= toOutput output
  QueryOwnActiveUTxOs network policyId addr output ->
    runQueryOwnActive network policyId addr >>= toOutput output
  QuerySpecificContract network policyId contractID output ->
    runQuerySpecificContract network policyId contractID >>= toOutput output
  QueryOwnContracts network policyId addr output ->
    runQueryOwnContracts network policyId addr >>= toOutput output

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
convertToAddressInfo :: InspectAddress -> AddressInfo'
convertToAddressInfo (InspectAddressShelley AddressInfo{..}) = AddressInfo'
    { addressSpendingKeyHash = convertByteString <$> infoSpendingKeyHash
    , addressSpendingScriptHash = convertByteString <$> infoScriptHash
    , addressStakeKeyHash = convertByteString <$> infoStakeKeyHash
    , addressStakeScriptHash = convertByteString <$> infoStakeScriptHash
    , addressNetworkTag = unNetworkTag infoNetworkTag
    }
  where convertByteString = decodeUtf8 . E.encode E.EBase16
convertToAddressInfo _ = error "This is not a shelley address."

toOutput :: (ToJSON a) => Output -> a -> IO ()
toOutput output xs = case output of
  Stdout -> BL.putStr $ encode xs
  File file -> BL.writeFile file $ encodePretty xs

extractAddressInfo :: Text -> Output -> IO ()
extractAddressInfo addr output = do
  let mAddr = fromBech32 addr
      inspect addr' = convertToAddressInfo <$> eitherInspectAddress Nothing addr'
  case (mAddr,output) of
    (Nothing,_) -> 
      putStrLn "Not a valid bech32 address."
    (Just x, File file) -> 
      BL.writeFile file $ encodePretty $ unsafeFromRight $ inspect x
    (Just x, Stdout) -> 
      BL.putStr $ encode $ unsafeFromRight $ inspect x

-- | This is hardcoded to only generate addresses for the Preprod testnet.
generateBech32Address :: Address -> Output -> IO ()
generateBech32Address (Address paymentCred mStakeCred) output = do
  let Right tag = mkNetworkDiscriminant 0 -- ^ Preproduction Testnet
      bechAddr = case (paymentCred,mStakeCred) of
        (PubKeyCredential pkh, Nothing) ->
          let Just hash = keyHashFromBytes (Payment, getPubKeyHash pkh)
          in bech32 $ paymentAddress tag (PaymentFromKeyHash hash)
        (ScriptCredential vh, Nothing) ->
          let Just scriptHash = scriptHashFromBytes $ getValidatorHash vh
          in bech32 $ paymentAddress tag (PaymentFromScript scriptHash)
        (PubKeyCredential pkh, Just (StakingHash (PubKeyCredential spkh))) ->
          let Just pHash = keyHashFromBytes (Payment, getPubKeyHash pkh)
              Just sHash = keyHashFromBytes (Delegation, getPubKeyHash spkh)
          in bech32 $ 
              delegationAddress tag (PaymentFromKeyHash pHash) (DelegationFromKeyHash sHash)
        (PubKeyCredential pkh, Just (StakingHash (ScriptCredential vh))) ->
          let Just pHash = keyHashFromBytes (Payment, getPubKeyHash pkh)
              Just scriptHash = scriptHashFromBytes $ getValidatorHash vh
          in bech32 $ 
              delegationAddress tag (PaymentFromKeyHash pHash) (DelegationFromScript scriptHash)
        (ScriptCredential vh, Just (StakingHash (PubKeyCredential pkh))) ->
          let Just scriptHash = scriptHashFromBytes $ getValidatorHash vh
              Just sHash = keyHashFromBytes (Delegation, getPubKeyHash pkh)
          in bech32 $ 
              delegationAddress tag (PaymentFromScript scriptHash) (DelegationFromKeyHash sHash)
        (ScriptCredential pvh, Just (StakingHash (ScriptCredential svh))) -> 
          let Just pScriptHash = scriptHashFromBytes $ getValidatorHash pvh
              Just sScriptHash = scriptHashFromBytes $ getValidatorHash svh
          in bech32 $ 
              delegationAddress tag (PaymentFromScript pScriptHash) (DelegationFromScript sScriptHash)
        _ -> error "Not a valid address."
  case output of
    Stdout -> TIO.putStrLn bechAddr
    File file -> TIO.writeFile file bechAddr