use aiken/dict.{Dict}
use aiken/list
use aiken/option
use aiken/time.{PosixTime}
use aiken/transaction.{Datum,Output}
use aiken/transaction/credential.{Address,ScriptCredential}
use aiken/transaction/value.{AssetName,PolicyId,Value}

use cardano_options/data/core.
  { ScriptHash
  , ProposalBeaconId
  , ActiveBeaconId
  , Asset
  , OfferBeacon
  , AskBeacon
  , TradingPairBeacon
  , PremiumAssetBeacon
  , Terms
  }
use cardano_options/data/datums.{ProposalDatum}
use cardano_options/utils.{error_if_false,trace_if_false,parse_datum}

// Check whether the terms are valid.
fn is_valid_terms(upper_bound: PosixTime, terms: Terms) -> Bool {
  let Terms{expiration,strike_price: (strike_num,strike_den),premium} = terms

  and {
    trace_if_false(@"Invalid possible expiration", expiration >= upper_bound),
    trace_if_false(@"Invalid possible strike_price", strike_num > 0 && strike_den > 0),
    trace_if_false(@"Invalid possible premium", premium > 0)
  }
}

// This is used to check that the value of Proposal UTxOs are correct. It returns the value of the
// offer asset so that it can be checked against the offer_quantity in the datum. When ada is the
// offer asset, the first Int should be offer_quantity + contract_deposit. Due to the way this
// function is written, beacon tokens do not count towards the offer quantities even if they
// are the offer asset. Therefore, beacon tokens cannot be used as the offer asset.
fn extract_proposal_quantity(
  val: Value,
  proposal_beacon_id: ProposalBeaconId,
  offer_beacon: OfferBeacon,
  ask_beacon: AskBeacon,
  trading_pair_beacon: TradingPairBeacon,
  premium_beacon: PremiumAssetBeacon,
  offer_asset: Asset
) -> (Int,Int) {
  // Extract out the offer asset info.
  let (offer_asset_id,offer_asset_name) = offer_asset

  // A function to map over the value of this UTxO. It will return the total number of the offer
  // asset and the total amount of ada.
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: (Int,Int)) {
    let (cn,tns_dict) = x
    let (offer_quantity,ada_quantity) = acc

    // Check the beacons.
    if cn == core.from_proposal_beacon_id(proposal_beacon_id) {
      let tns_list = dict.to_list(tns_dict)

      // A function to check each beacon.
      let beacon_check = fn(y: (AssetName,Int), inner_acc: (Bool,Bool,Bool,Bool)) {
        let (tn,n) = y
        let (offer_check,ask_check,trading_check,premium_check) = inner_acc
        if tn == core.get_offer_beacon_name(offer_beacon) {
          // There must only be one Offer beacon. If n was zero, tn could not have matched.
          ( error_if_false(@"UTxO has too many Offer beacons", n == 1)
          , ask_check
          , trading_check
          , premium_check
          )
        } else if tn == core.get_ask_beacon_name(ask_beacon) {
          // There must only be one Ask beacon. If n was zero, tn could not have matched.
          ( offer_check
          , error_if_false(@"UTxO has too many Ask beacons", n == 1)
          , trading_check
          , premium_check
          )
        } else if tn == core.get_trading_pair_beacon_name(trading_pair_beacon) {
          // There must only be one TradingPair beacon. If n was zero, tn could not have matched.
          ( offer_check
          , ask_check
          , error_if_false(@"UTxO has too many TradingPair beacons", n == 1)
          , premium_check
          )
        } else if tn == core.get_premium_asset_beacon_name(premium_beacon) {
          // There must only be one PremiumAsset beacon. If n was zero, tn could not have matched.
          ( offer_check
          , ask_check
          , trading_check
          , error_if_false(@"UTxO has too many PremiumAsset beacons", n == 1)
          )
        } else {
          error @"UTxO has a beacon with the wrong name"
        }
      }

      when list.foldl(tns_list,(False,False,False,False),beacon_check) is {
        (True,True,True,True) -> (offer_quantity,ada_quantity)

        // This path can be taken if the UTxO has only some of the required beacons. This path
        // should never be taken since the script should fail earlier if the UTxO has too few
        // or too many types of beacons.
        _ -> error @"UTxO does not have the right beacons"
      }
    } 

    // Check that the asset is the offer asset. Ada is the only asset that can be present without 
    // being the offer asset.
    else if cn == offer_asset_id {
      when dict.to_list(tns_dict) is {
        // Since this check assumes the offer asset is the only asset in the UTxO with that
        // policy id, beacons cannot be the offer asset. When ada is the offer asset, the 
        // ada_quantity field in the accumulator will not be used.
        [(tn,n)] -> {
          if tn == offer_asset_name {
            (offer_quantity+n,ada_quantity)
          } else {
            error @"No extraneous assets allowed in the UTxO"
          }
        }
        _ -> error @"No extraneous assets allowed in the UTxO"
      }
    }

    // Check that the asset is ada. If this branch is taken, then ada is not the offer asset.
    // Ada must be explicitly allowed to satisfy the minUTxOValue requirement.
    else if cn == #"" {
      expect Some(n) = dict.get(tns_dict,#"")

      (offer_quantity,ada_quantity+n)
    }

    else { error @"No extraneous assets allowed in the UTxO" }
  }

  // ada_quantity will be 0 when ada is the offer asset.
  list.foldl(value.to_dict(val) |> dict.to_list(_), (0,0), foo)
}

// This checks the datum and calls extract_proposal_quantity to check the value.
fn valid_proposal_output(
  expected_proposal_beacon_id: ProposalBeaconId,
  expected_active_beacon_id: ActiveBeaconId,
  proxy_hash: ScriptHash,
  upper_bound: PosixTime,
  output_value: Value,
  output_datum: Datum
) -> Bool {
  // Extract the info from the ProposalDatum. Crash the script if it is not an ProposalDatum.
  expect ProposalDatum{
    proposal_beacon_policy_id,
    active_beacon_policy_id,
    offer_asset: (offer_policy_id,_) as offer_asset,
    offer_quantity,
    ask_asset: (ask_policy_id,_) as ask_asset,
    trading_pair_beacon,
    offer_asset_beacon,
    ask_asset_beacon,
    premium_asset: (premium_policy_id,_) as premium_asset,
    premium_asset_beacon,
    contract_deposit,
    payment_address,
    possible_terms
  } = when parse_datum(output_datum) is {
    ProposalDatum{..} as datum -> datum
    _ -> error @"UTxO not stored with an ProposalDatum"
  }

  // Generate the required beacon names based on the trading pair. Direction matters.
  let (expected_offer_beacon,expected_ask_beacon,expected_trading_pair_beacon) = 
    core.gen_pair_beacons(offer_asset, ask_asset)

  let expected_premium_asset_beacon = core.gen_premium_asset_beacon(premium_asset)

  // Get the amount of the offer asset and the amount of ada in the UTxO. When ada is the offer
  // asset, the ada_quantity will be unused.
  let (actual_offer_quantity,ada_quantity) = 
    extract_proposal_quantity(
      output_value,
      expected_proposal_beacon_id,
      expected_offer_beacon,
      expected_ask_beacon,
      expected_trading_pair_beacon,
      expected_premium_asset_beacon,
      offer_asset
    )

  // All of the following must be true.
  and {
    // Datum checks:
    trace_if_false(
      @"Datum has wrong proposal beacon id", 
      expected_proposal_beacon_id == core.to_proposal_beacon_id(proposal_beacon_policy_id)),
    trace_if_false(
      @"Datum has wrong active beacon id", 
      expected_active_beacon_id == core.to_active_beacon_id(active_beacon_policy_id)),
    trace_if_false(
      @"Offer asset cannot be protocol beacon", 
      offer_policy_id != core.from_proposal_beacon_id(expected_proposal_beacon_id) &&
      offer_policy_id != core.from_active_beacon_id(expected_active_beacon_id)),
    trace_if_false(
      @"Datum has invalid offer quantity", 
      offer_quantity > 0),
    trace_if_false(
      @"Ask asset cannot be protocol beacon", 
      ask_policy_id != core.from_proposal_beacon_id(expected_proposal_beacon_id) &&
      ask_policy_id != core.from_active_beacon_id(expected_active_beacon_id)),
    trace_if_false(
      @"Datum has wrong TradingPair beacon",
      trading_pair_beacon == core.get_trading_pair_beacon_name(expected_trading_pair_beacon)),
    trace_if_false(
      @"Datum has wrong Offer beacon",
      offer_asset_beacon == core.get_offer_beacon_name(expected_offer_beacon)),
    trace_if_false(
      @"Datum has wrong Ask beacon",
      ask_asset_beacon == core.get_ask_beacon_name(expected_ask_beacon)),
    trace_if_false(
      @"Datum has wrong PremiumAsset beacon",
      premium_asset_beacon == core.get_premium_asset_beacon_name(expected_premium_asset_beacon)),
    trace_if_false(
      @"Premium asset cannot be protocol beacon", 
      premium_policy_id != core.from_proposal_beacon_id(expected_proposal_beacon_id) &&
      premium_policy_id != core.from_active_beacon_id(expected_active_beacon_id)),
    trace_if_false(
      @"Invalid contract deposit",
      contract_deposit > 0),
    trace_if_false(
      @"Invalid payment address",
      utils.valid_payment_address(proxy_hash,payment_address)),
    // Check the possible_terms. This has its own trace messages.
    list.all(possible_terms,fn(x) {is_valid_terms(upper_bound,x)}),
    trace_if_false(
      @"Offer asset cannot be same as ask asset",
      offer_asset != ask_asset),

    // Value checks:
    trace_if_false(
      @"UTxO has wrong value",
      if offer_asset == (#"",#"") { 
        actual_offer_quantity == offer_quantity + contract_deposit
      } else { 
        ada_quantity == contract_deposit && actual_offer_quantity == offer_quantity
      }
    )
  }
}


// Check if the beacons are locked at the proper address and the Proposal UTxO is configured properly.
// This function can return False.
pub fn proposal_beacons_destination_check(
  options_script_hash: ScriptHash,
  proposal_beacon_id: ProposalBeaconId,
  active_beacon_id: ActiveBeaconId,
  proxy_hash: ScriptHash,
  outputs: List<Output>,
  upper_bound: PosixTime
) -> Bool {
  // A function to map over the list of outputs.
  let foo = fn (out: Output, acc: Bool) {
    // Extract out the output's information.
    let Output(Address(pay,stake),val,d,_) = out

    // Get the beacons in this UTxO and pattern match on it.
    when value.tokens(val,core.from_proposal_beacon_id(proposal_beacon_id)) |> dict.to_list() is {
      // If beacons are not present, this output can be ignored.
      [] -> acc

      // The UTxO has four types of beacon. The exact beacons will be checked later.
      [(_,_),(_,_),(_,_),(_,_)] -> {
        and {
          // All previous outputs must be valid.
          acc,

          // The output must be at a DApp address with a valid staking credential.
          trace_if_false( 
            @"Beacon must go to a DApp address using a valid staking credential",
            ScriptCredential(options_script_hash) == pay && option.is_some(stake)
          ),

          // The output must be properly configured. This has its own trace messages.
          valid_proposal_output(proposal_beacon_id,active_beacon_id,proxy_hash,upper_bound,val,d)
        }
      }

      // This path can only be taken if the above two matches failed which means the UTxO
      // has the wrong number of beacons.
      _ -> error @"Proposal UTxOs must have exactly four kinds of beacons"
    }
  }

  // Map over the list. This will only return True if all the outputs are valid.
  list.foldl(outputs, True, foo)
}
