{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Query
(
  runQueryAvailableContracts
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception

import CLI.BlockfrostApi as Blockfrost
import CLI.Types
import CardanoOptions (CurrencySymbol())

runQueryAvailableContracts :: Network -> CurrencySymbol -> IO [AvailableContractInfo]
runQueryAvailableContracts (PreProdTestnet apiKey) currSym = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryAvailableContracts apiKey' (show currSym)) env
  case res of
    Right r -> return r
    Left err -> throw err