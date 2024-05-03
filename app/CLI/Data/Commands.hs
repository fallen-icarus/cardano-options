{-# LANGUAGE StrictData #-}

module CLI.Data.Commands where

import Relude

import CardanoOptions

import CLI.Data.ApiService
import CLI.Data.Bech32Address
import CLI.Data.Network
import CLI.Data.Output

data Command
  = ExportScript Script FilePath
  | CreateDatum NewDatum FilePath
  | CreateRedeemer NewRedeemer FilePath
  | BeaconName BeaconName Output
  | ConvertTime ConvertTime Network
  | Query Query
  | SubmitTx Network ApiService FilePath
  | EvaluateTx Network ApiService FilePath
  | ExportParams Network Output

data Script 
  = ProposalBeaconScript
  | ActiveBeaconScript
  | AddressUpdateObserverScript
  | OptionsScript
  | ProxyScript

data ConvertTime
  = POSIXTimeToSlot POSIXTime
  | SlotToPOSIXTime Slot

data NewDatum
  = NewProposalDatum NewProposalInfo
  | NewActiveDatumManual NewActiveInfo
  | NewActiveDatumAuto Network ApiService Int TxOutRef
  | NewPostAddressUpdateActiveDatumManual NewAddressInfo
  | NewPostAddressUpdateActiveDatumAuto Network ApiService TxOutRef Address
  | NewPaymentDatum ContractId

data NewRedeemer
  = NewProposalRedeemer ProposalBeaconsRedeemer
  | NewOptionsRedeemer OptionsRedeemer
  | NewActiveRedeemer ActiveBeaconsRedeemer
  | NewAddressObserverRedeemer AddressObserverRedeemer

data BeaconName
  = ProposalPolicyId
  | ActivePolicyId
  | OfferBeaconName OfferAsset
  | AskBeaconName AskAsset
  | PremiumBeaconName PremiumAsset
  | TradingPairBeaconName OfferAsset AskAsset
  | ContractIdName TxOutRef

data Query
  -- | Query a user's personal address. You can optionally filter for only UTxOs with a Key NFT.
  = QueryPersonal Network ApiService PaymentAddress Bool Format Output
  -- | Query the latest slot number.
  | QueryCurrentSlot Network ApiService
  -- | Query all Proposal UTxOs. You must filter by at least one beacon, and you can optionally
  -- filter by writer address.
  | QueryProposals 
      Network 
      ApiService 
      (Maybe OfferAsset) 
      (Maybe AskAsset) 
      (Maybe PremiumAsset) 
      (Maybe PaymentAddress) 
      Format Output
  -- | Query all Active UTxOs. You must filter by at least one beacon, and you can optionally
  -- filter by writer address.
  | QueryActives
      Network 
      ApiService 
      (Maybe OfferAsset) 
      (Maybe AskAsset) 
      (Maybe ContractId) 
      (Maybe PaymentAddress) 
      Format Output
