{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

module CLI.Query
  ( runQuerySlotTip
  , runQueryPersonalAddress
  , runQueryProposals
  , runQuerySpecificOptionsUTxO
  , runSubmitTx
  , runEvaluateTx
  ) where

import Relude
import Servant.Client 
import Network.HTTP.Client hiding (responseBody)
import Network.HTTP.Client.TLS
import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson

import CLI.Data.ApiService
import CLI.Data.Bech32Address
import CLI.Data.Network
import CLI.Data.OptionsUTxO
import CLI.Data.PersonalUTxO
import CLI.Data.TxCBOR
import CLI.Query.Koios as Koios

import CardanoOptions

runQuerySlotTip :: Network -> ApiService -> IO Integer
runQuerySlotTip network api = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM Koios.querySlotTip env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM Koios.querySlotTip env

runQueryPersonalAddress :: Network -> ApiService -> PaymentAddress -> Bool -> IO [PersonalUTxO]
runQueryPersonalAddress network api addr keysOnly = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryPersonalAddress addr keysOnly) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryPersonalAddress addr keysOnly) env

runQueryProposals 
  :: Network 
  -> ApiService
  -> [(CurrencySymbol,TokenName)]
  -> Maybe PaymentAddress 
  -> IO [OptionsUTxO]
runQueryProposals network api targetBeacons mWriterAddr = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryProposals targetBeacons mWriterAddr) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryProposals targetBeacons mWriterAddr) env

runQuerySpecificOptionsUTxO :: Network -> ApiService -> TxOutRef -> IO [OptionsUTxO]
runQuerySpecificOptionsUTxO network api outRef = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.querySpecificOptionsUTxO outRef) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.querySpecificOptionsUTxO outRef) env

runSubmitTx :: Network -> ApiService -> FilePath -> IO Value
runSubmitTx network api txFile = do
  tx' <- decode @TxCBOR <$> LBS.readFile txFile
  case tx' of
    Nothing -> return "Failed to deserialise transaction file"
    Just tx -> do
      manager' <- newManager tlsManagerSettings
      res <- case (network,api) of
        (PreProdTestnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.submitTx tx) env
        (Mainnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.submitTx tx) env
      case res of
        Right r -> return r
        Left e@(FailureResponse _ err) -> case decode $ responseBody err of
          Just response -> return response
          Nothing -> throw e
        Left err -> throw err

runEvaluateTx :: Network -> ApiService -> FilePath -> IO Value
runEvaluateTx network api txFile = do
  tx' <- decode @TxCBOR <$> LBS.readFile txFile
  case tx' of
    Nothing -> return "Failed to deserialise transaction file"
    Just tx -> do
      manager' <- newManager tlsManagerSettings
      res <- case (network,api) of
        (PreProdTestnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.evaluateTx tx) env
        (Mainnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.evaluateTx tx) env
      case res of
        Right r -> return r
        Left e@(FailureResponse _ err) -> case decode $ responseBody err of
          Just response -> return response
          Nothing -> throw e
        Left err -> throw err
