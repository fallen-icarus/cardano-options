use aiken/bytearray as byte
use aiken/hash.{Blake2b_224,Hash}
use aiken/transaction/credential. {Script , VerificationKey}
use aiken/transaction/value.{PolicyId,AssetName}
use aiken/transaction.{OutputReference,TransactionId}
use aiken/time.{PosixTime}
use aiken/string

//-----------------------------------------------
// Type Synonyms
//-----------------------------------------------
pub type Asset = (PolicyId,AssetName)
pub type ScriptHash = Hash<Blake2b_224,Script>
pub type KeyHash = Hash<Blake2b_224,VerificationKey>
pub type Fraction = (Int,Int)

//-----------------------------------------------
// Proposal Id
//-----------------------------------------------
// A zero cost type representing the proposal beacon script's policy id.
pub opaque type ProposalBeaconId {
  inner: PolicyId
}

// Cast a policy id to a ProposalBeaconId.
pub fn to_proposal_beacon_id(policy_id: PolicyId) -> ProposalBeaconId {
  ProposalBeaconId(policy_id)
}

// Get the policy id from a ProposalBeaconId.
pub fn from_proposal_beacon_id(policy_id: ProposalBeaconId) -> PolicyId {
  policy_id.inner
}

//-----------------------------------------------
// Active Id
//-----------------------------------------------
// A type representing the active beacon script's policy id.
pub opaque type ActiveBeaconId {
  inner: PolicyId
}

// Cast a policy id to an ActiveBeaconId.
pub fn to_active_beacon_id(policy_id: PolicyId) -> ActiveBeaconId {
  ActiveBeaconId(policy_id)
}

// Get the policy id from an ActiveBeaconId.
pub fn from_active_beacon_id(policy_id: ActiveBeaconId) -> PolicyId {
  policy_id.inner
}

//-----------------------------------------------
// TradingPair Beacon
//-----------------------------------------------
// A zero cost type representing the TradingPair beacon name.
pub opaque type TradingPairBeacon {
  inner: AssetName
}

// Get the TradingPair beacon's asset name from the opaque type.
pub fn get_trading_pair_beacon_name(trading_pair_beacon: TradingPairBeacon) -> AssetName {
  trading_pair_beacon.inner
}

// Convert an AssetName directly to a TradingPairBeacon. This should only be used
// when the AssetName is guaranteed to be an TradingPairBeacon.
pub fn unsafe_to_trading_pair_beacon(trading_pair_beacon: AssetName) -> TradingPairBeacon {
  TradingPairBeacon(trading_pair_beacon)
}

//-----------------------------------------------
// Offer Beacon
//-----------------------------------------------
// A zero cost type representing the Offer beacon name.
pub opaque type OfferBeacon {
  inner: AssetName
}

// Get the Offer beacon's asset name from the opaque type.
pub fn get_offer_beacon_name(offer_beacon: OfferBeacon) -> AssetName {
  offer_beacon.inner
}

// Convert an AssetName directly to an OfferBeacon. This should only be used
// when the AssetName is guaranteed to be an OfferBeacon.
pub fn unsafe_to_offer_beacon(offer_beacon: AssetName) -> OfferBeacon {
  OfferBeacon(offer_beacon)
}

//-----------------------------------------------
// Ask Beacon
//-----------------------------------------------
// A zero cost type representing the Ask beacon name.
pub opaque type AskBeacon {
  inner: AssetName
}

// Get the Ask beacon's asset name from the opaque type.
pub fn get_ask_beacon_name(ask_beacon: AskBeacon) -> AssetName {
  ask_beacon.inner
}

// Convert an AssetName directly to an AskBeacon. This should only be used
// when the AssetName is guaranteed to be a AskBeacon.
pub fn unsafe_to_ask_beacon(ask_beacon: AssetName) -> AskBeacon {
  AskBeacon(ask_beacon)
}

//-----------------------------------------------
// PremiumAsset Beacon
//-----------------------------------------------
// A zero cost type representing the PremiumAsset beacon name.
pub opaque type PremiumAssetBeacon {
  inner: AssetName
}

// Get the PremiumAsset beacon's asset name from the opaque type.
pub fn get_premium_asset_beacon_name(premium_asset_beacon: PremiumAssetBeacon) -> AssetName {
  premium_asset_beacon.inner
}

//-----------------------------------------------
// Beacon Names
//-----------------------------------------------
// Generate the beacon asset names from a trading pair.
pub fn gen_pair_beacons(
  offer: Asset, 
  ask: Asset
) -> (OfferBeacon,AskBeacon,TradingPairBeacon) {
  let (offer_id,offer_name) = offer
  let (ask_id,ask_name) = ask

  let corrected_offer_id = if offer_id == #"" { #"00" } else { offer_id }
  let corrected_ask_id = if ask_id == #"" { #"00" } else { ask_id }

  let trading_pair_beacon =
    byte.concat(corrected_offer_id,offer_name) // Concat the offer asset name.
    |> byte.concat(_,corrected_ask_id) // Append the ask asset id.
    |> byte.concat(_,ask_name) // Append the ask asset name.
    |> hash.sha2_256(_) // Hash the result.
    |> TradingPairBeacon // Wrap it in the opaque type.

  let offer_beacon =
    byte.concat(offer_id,offer_name) // Concat the asset name.
    |> byte.concat(#"01",_) // Prefix it.
    |> hash.sha2_256(_) // Hash the prefixed name.
    |> OfferBeacon // Wrap it in the opaque type.

  let ask_beacon =
    byte.concat(ask_id,ask_name) // Concat the asset name.
    |> byte.concat(#"02",_) // Prefix it.
    |> hash.sha2_256(_) // Hash the prefixed name.
    |> AskBeacon // Wrap it in the opaque type.

  (offer_beacon,ask_beacon,trading_pair_beacon)
}

// Generate the PremiumAsset beacon name.
pub fn gen_premium_asset_beacon(premium_asset: Asset) -> PremiumAssetBeacon {
  let (premium_id,premium_name) = premium_asset

  byte.concat(premium_id,premium_name) // Concat the asset name.
  |> byte.concat(#"03",_) // Prefix it.
  |> hash.sha2_256(_) // Hash the prefixed name.
  |> PremiumAssetBeacon // Wrap it in the opaque type.
}

//-----------------------------------------------
// Contract Terms
//-----------------------------------------------
// The type capturing what option writers can vary when creating an assortment of
// contracts to choose from.
pub type Terms {
  premium: Int,
  strike_price: Fraction,
  expiration: PosixTime
}

//-----------------------------------------------
// Contract Ids
//-----------------------------------------------
// A type representing contract ids. It is written as an opaque type to create a zero cost type
// that only exists at compile time.
pub opaque type ContractId {
  inner: AssetName
}

// Get the inner AssetName from the ContractId.
pub fn get_contract_id_name(contract_id: ContractId) -> AssetName {
  contract_id.inner
}

// Create the ContractId from the Proposal's `OuptutReference` by hashing the combination of the tx hash
// and the output index. It uses the sha2_256 hash algorithm. 
pub fn gen_contract_id(offer_ref: OutputReference) -> ContractId {
  let OutputReference(TransactionId(tx_hash),out_index) = offer_ref
  string.from_int(out_index)
  |> byte.from_string(_)
  |> byte.concat(tx_hash,_)
  |> hash.sha2_256(_)
  |> ContractId
}

// Convert an AssetName directly to a ContractId. This should only be used
// when the AssetName is guaranteed to be a ContractId.
pub fn unsafe_to_contract_id(contract_id: AssetName) -> ContractId {
  ContractId(contract_id)
}
