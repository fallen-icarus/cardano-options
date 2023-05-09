{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module CLI.BlockfrostApi
(
  BlockfrostApiKey(..),

  queryAvailableContracts,
  queryOwnAssets,
  queryOwnProposals,
  queryOwnActive,
  querySpecificContract
) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Control.Monad
import qualified Data.Text as T
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust,fromJust)

import CLI.Types
import CardanoOptions

-------------------------------------------------
-- Core Types
-------------------------------------------------
-- | Newtype wrapper around api key for using blockfrost
newtype BlockfrostApiKey = BlockfrostApiKey String

instance ToHttpApiData BlockfrostApiKey where
  toQueryParam (BlockfrostApiKey apiKey) = T.pack apiKey

-- | Newtype wrapper around the beacon asset being queried.
data BeaconId = BeaconId (String,String)

instance ToHttpApiData BeaconId where
  toQueryParam (BeaconId (currSym,tokName)) = T.pack $ currSym <> tokName

-- | An address that contains a beacon.
-- The response type of the beaconAddressList api.
newtype BeaconAddress = BeaconAddress { unBeaconAddress :: String } deriving (Show)

instance FromJSON BeaconAddress where
  parseJSON (Object o) = BeaconAddress <$> o .: "address"
  parseJSON _ = mzero

instance ToHttpApiData BeaconAddress where
  toQueryParam = T.pack . unBeaconAddress

-- | The response type of the beaconInfoApi. This has all the information that may be needed
-- depending on the beacon being queried.
data RawBeaconInfo = RawBeaconInfo
  { rawTxHash :: String
  , rawOutputIndex :: Integer
  , rawAmount :: [RawAssetInfo]
  , rawBeaconDataHash :: Maybe String
  } deriving (Show)

instance FromJSON RawBeaconInfo where
  parseJSON (Object o) =
    RawBeaconInfo
      <$> o .: "tx_hash"
      <*> o .: "tx_index"
      <*> o .: "amount"
      <*> o .: "data_hash"
  parseJSON _ = mzero

-- | Blockfrost does not separate symbol and name with '.'
data RawAssetInfo = RawAssetInfo
  { rawUnit :: String  -- ^ CurrencySymbol <> TokenName
  , rawQuantity :: Integer
  } deriving (Show)

instance FromJSON RawAssetInfo where
  parseJSON (Object o) =
    RawAssetInfo
      <$> o .: "unit"
      <*> fmap read (o .: "quantity")
  parseJSON _ = mzero

instance FromJSON OptionsDatum where
  parseJSON (Object o) = do
    r <- o .: "json_value" >>= return . decodeDatum
    case r of
      Just x -> return x
      Nothing -> mzero
  parseJSON _ = mzero

-------------------------------------------------
-- Blockfrost Api
-------------------------------------------------
type BlockfrostApi
  =    "assets"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "asset" BeaconId
    :> "addresses"
    :> Get '[JSON] [BeaconAddress]

  :<|> "addresses"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "address" BeaconAddress
    :> "utxos"
    :> Capture "asset" BeaconId
    :> Get '[JSON] [RawBeaconInfo]

  :<|> "scripts"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> "datum"
    :> Capture "datum_hash" String
    :> Get '[JSON] Value

beaconAddressListApi :<|> beaconInfoApi :<|> datumApi = client api
  where
    api :: Proxy BlockfrostApi
    api = Proxy

-------------------------------------------------
-- Blockfrost Query Functions
-------------------------------------------------
queryAvailableContracts :: BlockfrostApiKey -> String -> ClientM [AvailableContractInfo]
queryAvailableContracts apiKey policyId = do
  let assetsBeaconId = BeaconId (policyId,"417373657473")
      proposedBeaconId = BeaconId (policyId,"50726f706f736564")
  -- | Get all the addresses currently holding the assets beacon.
  addrs <- beaconAddressListApi apiKey assetsBeaconId
  -- | Get all the assets beacon UTxOs for those addresses.
  assetsUTxOs <- mapM (\z -> beaconInfoApi apiKey z assetsBeaconId) addrs
  -- | Get all the proposed beacon UTxOs for those addresses.
  proposalUTxOs <- mapM (\z -> beaconInfoApi apiKey z proposedBeaconId) addrs
  -- | Get all the assets datums attached to the assets UTxOs.
  assetsInfo <- mapM (fetchDatumsLenient apiKey) $ map (map rawBeaconDataHash) assetsUTxOs
  -- | Get all the proposed datums attached to the proposed UTxOs.
  proposedInfo <- mapM (fetchDatumsLenient apiKey) $ map (map rawBeaconDataHash) proposalUTxOs
  let -- | Convert the assets UTxO info to the user friendly format.
      assets = zipWith convertToUTxOInfo assetsUTxOs assetsInfo
      -- | Convert the proposed UTxO info to the user friendly format.
      proposed = zipWith convertToUTxOInfo proposalUTxOs proposedInfo
  return $ concat $ zipWith3 convertToAvailableContractInfo addrs assets proposed

queryOwnAssets :: BlockfrostApiKey -> String -> String -> ClientM [UTxOInfo]
queryOwnAssets apiKey policyId addr = do
  let beaconAddr = BeaconAddress addr
      beaconId = BeaconId (policyId,"417373657473")
  -- | Get all the beacon UTxOs for the address.
  beaconUTxOs <- beaconInfoApi apiKey beaconAddr beaconId
  -- | Get all the datums attached to the beacon UTxOs.
  infos <- fetchDatumsLenient apiKey $ map rawBeaconDataHash beaconUTxOs
  return $ convertToUTxOInfo beaconUTxOs infos

queryOwnProposals :: BlockfrostApiKey -> String -> String -> ClientM [UTxOInfo]
queryOwnProposals apiKey policyId addr = do
  let beaconAddr = BeaconAddress addr
      beaconId = BeaconId (policyId,"50726f706f736564")
  -- | Get all the beacon UTxOs for the address.
  beaconUTxOs <- beaconInfoApi apiKey beaconAddr beaconId
  -- | Get all the datums attached to the beacon UTxOs.
  infos <- fetchDatumsLenient apiKey $ map rawBeaconDataHash beaconUTxOs
  return $ convertToUTxOInfo beaconUTxOs infos

queryOwnActive :: BlockfrostApiKey -> String -> String -> ClientM [UTxOInfo]
queryOwnActive apiKey policyId addr = do
  let beaconAddr = BeaconAddress addr
      beaconId = BeaconId (policyId,"416374697665")
  -- | Get all the beacon UTxOs for the address.
  beaconUTxOs <- beaconInfoApi apiKey beaconAddr beaconId
  -- | Get all the datums attached to the beacon UTxOs.
  infos <- fetchDatumsLenient apiKey $ map rawBeaconDataHash beaconUTxOs
  return $ convertToUTxOInfo beaconUTxOs infos

querySpecificContract :: BlockfrostApiKey -> String -> String -> ClientM [UTxOInfo]
querySpecificContract apiKey policyId contractID = do
  let contractBeacon = BeaconId (policyId,contractID)
      activeBeacon = policyId <> "416374697665"
  -- | Get all the addresses currently holding the contractId beacon.
  -- There should only be at most two.
  addrs <- beaconAddressListApi apiKey contractBeacon
  -- | Get all the beacon UTxOs for the addresses.
  beaconUTxOs <- filterForAsset activeBeacon . concat 
             <$> mapM (\z -> beaconInfoApi apiKey z contractBeacon) addrs
  -- | Get all the datums attached to the beacon UTxOs.
  infos <- fetchDatumsLenient apiKey $ map rawBeaconDataHash beaconUTxOs
  return $ convertToUTxOInfo beaconUTxOs infos

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
filterForAsset :: String -> [RawBeaconInfo] -> [RawBeaconInfo]
filterForAsset asset = filter (isJust . find ((==asset) . rawUnit) . rawAmount)

-- | Skips ones that fail to decode.
fetchDatumsLenient :: BlockfrostApiKey -> [Maybe String] -> ClientM (Map String OptionsDatum)
fetchDatumsLenient apiKey dhs =
  let go _ datumMap [] = return datumMap
      go key datumMap ((Just d):ds) = do
        i' <- fromJSON <$> datumApi key d
        case i' of
          Success i -> go key (Map.insert d i datumMap) ds
          Error _ -> go key datumMap ds
      go key datumMap (Nothing:ds) = go key datumMap ds
  in go apiKey Map.empty dhs

convertToAsset :: RawAssetInfo -> Asset
convertToAsset RawAssetInfo{rawUnit=u,rawQuantity=q} =
  if u == "lovelace"
  then Asset
        { assetPolicyId = u
        , assetTokenName = ""
        , assetQuantity = q
        }
  else Asset
        { assetPolicyId = take 56 u  -- ^ The policy id is always 56 characters
        , assetTokenName = drop 56 u
        , assetQuantity = q
        }

convertToUTxOInfo :: [RawBeaconInfo] -> Map String OptionsDatum -> [UTxOInfo]
convertToUTxOInfo [] _ = []
convertToUTxOInfo ((RawBeaconInfo tx ix amount dHash):rs) datumMap =
    info : convertToUTxOInfo rs datumMap
  where info = UTxOInfo
                { txHash = tx
                , outputIndex = show ix
                , utxoValue = map convertToAsset amount
                , optionInfo = fromJust $ join $ fmap (\z -> Map.lookup z datumMap) dHash
                }

convertToAvailableContractInfo :: BeaconAddress -> [UTxOInfo] -> [UTxOInfo] -> [AvailableContractInfo]
convertToAvailableContractInfo _ [] _ = []
convertToAvailableContractInfo addr'@(BeaconAddress addr) (asset:assets) ps
  | null filterPs = convertToAvailableContractInfo addr' assets ps
  | otherwise = info : convertToAvailableContractInfo addr' assets ps
  where
    target = currentAssetQuantity $ optionInfo asset
    filterPs = filter (\z -> currentAssetQuantity (optionInfo z) == target) ps
    info = AvailableContractInfo
      { address = addr
      , assetsUTxO = asset
      , proposedUTxOs = filterPs
      }