{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Query
(
  runQueryAvailableContracts,
  runQueryOwnAssets,
  runQueryOwnProposals,
  runQueryOwnActive,
  runQuerySpecificContract
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception

import CLI.BlockfrostApi as Blockfrost
import CLI.Types
import CardanoOptions

runQueryAvailableContracts :: Network -> CurrencySymbol -> IO [AvailableContractInfo]
runQueryAvailableContracts (PreProdTestnet apiKey) currSym = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryAvailableContracts apiKey' (show currSym)) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryOwnAssets :: Network -> CurrencySymbol -> OptionsAddress -> IO [UTxOInfo]
runQueryOwnAssets (PreProdTestnet apiKey) currSym addr = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryOwnAssets apiKey' (show currSym) (show addr)) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryOwnProposals :: Network -> CurrencySymbol -> OptionsAddress -> IO [UTxOInfo]
runQueryOwnProposals (PreProdTestnet apiKey) currSym addr = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryOwnProposals apiKey' (show currSym) (show addr)) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryOwnActive :: Network -> CurrencySymbol -> OptionsAddress -> IO [UTxOInfo]
runQueryOwnActive (PreProdTestnet apiKey) currSym addr = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryOwnActive apiKey' (show currSym) (show addr)) env
  case res of
    Right r -> return r
    Left err -> throw err

runQuerySpecificContract :: Network -> CurrencySymbol -> TokenName -> IO [UTxOInfo]
runQuerySpecificContract (PreProdTestnet apiKey) currSym tokName = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM 
          (Blockfrost.querySpecificContract apiKey' (show currSym) (show $ tokenNameAsTxId tokName)) 
          env
  case res of
    Right r -> return r
    Left err -> throw err