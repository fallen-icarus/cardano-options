use aiken/transaction/credential.{Address}
use aiken/time.{PosixTime}
use aiken/transaction/value.{PolicyId,AssetName}

use cardano_options/data/core.
  { Asset
  , Fraction
  , ScriptHash
  , Terms
  }

pub type OptionsDatum {
  // The datum for an options contract that is available for purchase.
  ProposalDatum {
    // The policy id for the proposal beacon script.
    proposal_beacon_policy_id: PolicyId,
    // The policy id for the active beacon script.
    active_beacon_policy_id: PolicyId,
    // The asset being offered.
    offer_asset: Asset,
    // The amount of the offer asset being offered.
    offer_quantity: Int,
    // The asset being asked for.
    ask_asset: Asset,
    // The token name for the trading pair beacon.
    trading_pair_beacon: AssetName,
    // The token name for the offer beacon.
    offer_asset_beacon: AssetName,
    // The token name for the ask beacon.
    ask_asset_beacon: AssetName,
    // The asset the premium must be paid in.
    premium_asset: Asset,
    // The token name for the premium asset beacon.
    premium_asset_beacon: AssetName,
    // The amount the writer paid for the minUTxOValue.
    contract_deposit: Int,
    // The address where the premium must go upon purchase of the contract.
    payment_address: Address,
    // The possible terms the buyer can pick from.
    possible_terms: List<Terms>
  }

  // The datum for an options contract that has been purchased, and can be executed any time up
  // until the expiration.
  ActiveDatum {
    // The policy id for the proposal beacon script. This is needed to support using the same
    // active beacons redeemer for purchases, executions, and closing expired.
    proposal_beacon_policy_id: PolicyId,
    // The policy id for the active beacon script.
    active_beacon_policy_id: PolicyId,
    // The hash of the address update observer script.
    address_observer_hash: ScriptHash,
    // The asset being offered.
    offer_asset: Asset,
    // The amount of the offer asset being offered.
    offer_quantity: Int,
    // The asset being asked for.
    ask_asset: Asset,
    // The token name for the trading pair beacon.
    trading_pair_beacon: AssetName,
    // The token name for the offer beacon.
    offer_asset_beacon: AssetName,
    // The token name for the ask beacon.
    ask_asset_beacon: AssetName,
    // The strike price for this contract.
    strike_price: Fraction,
    // This contract's expiration.
    expiration: PosixTime,
    // The amount the writer paid for the minUTxOValue.
    contract_deposit: Int,
    // The address where the ask asset must go upon execution of the contract.
    payment_address: Address,
    // The unique identitier for this contract.
    contract_id: AssetName
  }
}
