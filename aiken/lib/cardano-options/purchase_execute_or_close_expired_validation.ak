use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/list
use aiken/time.{PosixTime}
use aiken/transaction.
  { Datum
  , InlineDatum
  , Output
  , OutputReference
  , Input
  , ScriptPurpose
  , Redeemer
  , Spend
  , Mint
  , WithdrawFrom
  , ValidityRange
  } as tx
use aiken/transaction/credential.{Address, Credential, ScriptCredential, Inline}
use aiken/transaction/value.{Value, PolicyId, AssetName}

use cardano_options/data/core.
  { ScriptHash
  , ProposalBeaconId
  , ActiveBeaconId
  , Asset
  , OfferBeacon
  , AskBeacon
  , TradingPairBeacon
  , ContractId
  , Terms
  , Fraction
  }
use cardano_options/data/datums.{ProposalDatum,ActiveDatum}
use cardano_options/data/redeemers.{OptionsRedeemer,ProposalBeaconsRedeemer}
use cardano_options/utils.{error_if_false}

// The information needed to validate the required new contract output during a purchase.
type RequiredContractOutput {
  active_beacon_id: ActiveBeaconId,
  trading_pair_beacon: TradingPairBeacon,
  offer_beacon: OfferBeacon,
  ask_beacon: AskBeacon,
  contract_id: ContractId,
  offer_asset: Asset,
  offer_quantity: Int,
  contract_deposit: Int,
  req_datum: Datum,
  req_address: Address
}

// The information needed to validate the required premium payment output during a purchase.
type RequiredPremiumOutput {
  active_beacon_id: ActiveBeaconId,
  contract_id: ContractId,
  payment_address: Address,
  premium_asset: Asset,
  premium: Int
}

// The information needed to validate the required execution payment output during an execution.
type RequiredExecutionOutput {
  active_beacon_id: ActiveBeaconId,
  contract_id: ContractId,
  payment_address: Address,
  ask_asset: Asset,
  strike_price: Fraction,
  offer_quantity: Int,
  contract_deposit: Int
}

// Get the proposal beacons burned and the active beacons minted.
fn get_beacon_mint_and_burn(
  proposal_beacon_id: ProposalBeaconId,
  active_beacon_id: ActiveBeaconId,
  acc: (Option<Dict<AssetName,Int>>,Option<Dict<AssetName,Int>>),
  xs: List<(PolicyId,Dict<AssetName,Int>)>
) -> (Option<Dict<AssetName,Int>>,Option<Dict<AssetName,Int>>) {
  // A custom function to collect both the proposal beacons and the active beacons in a single
  // pass. It will recurse until it finds both.
  when acc is {
    (Some(_),Some(_)) -> acc
    (propos,acts) -> {
      when xs is {
        [] -> acc
        [(cn,tns), ..rest] -> {
          if cn == core.from_proposal_beacon_id(proposal_beacon_id) {
            get_beacon_mint_and_burn(proposal_beacon_id,active_beacon_id,(Some(tns),acts),rest)
          } else if cn == core.from_active_beacon_id(active_beacon_id) {
            get_beacon_mint_and_burn(proposal_beacon_id,active_beacon_id,(propos,Some(tns)),rest)
          } else {
            get_beacon_mint_and_burn(proposal_beacon_id,active_beacon_id,acc,rest)
          }
        }
      }
    }
  }
}

// This is used to check that the value of execution payment UTxOs are correct. It returns the value
// of the ask asset so that it can be checked against the ask_quantity in the datum. When ada is the
// ask asset, the first Int should be ask_quantity + contract_deposit.
fn extract_ask_quantity(
  val: Value,
  ask_asset: Asset
) -> (Int,Int) {
  // Extract out the ask asset info.
  let (ask_asset_id,ask_asset_name) = ask_asset

  // A function to map over the value of this UTxO. It will return the total number of the ask
  // asset and the total amount of ada.
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: (Int,Int)) {
    let (cn,tns_dict) = x
    let (ask_quantity,ada_quantity) = acc

    // Check that the asset is the ask asset. Ada is the only asset that can be present without 
    // being the ask asset.
    if cn == ask_asset_id {
      when dict.to_list(tns_dict) is {
        // When ada is the ask asset, the ada_quantity field in the accumulator will not be used.
        [(tn,n)] -> {
          expect True = error_if_false(
            @"No extraneous assets allowed in the UTxO",
            tn == ask_asset_name
          )
            
          (ask_quantity+n, ada_quantity)
        }
        _ -> error @"No extraneous assets allowed in the UTxO"
      }
    }

    // Check that the asset is ada. If this branch is taken, then ada is not the ask asset.
    else if cn == #"" {
      expect Some(n) = dict.get(tns_dict,#"")

      (ask_quantity,ada_quantity+n)
    }

    else { error @"No extraneous assets allowed in the UTxO" }
  }

  // ada_quantity will be 0 when ada is the ask asset.
  list.foldl(value.to_dict(val) |> dict.to_list(_), (0,0), foo)
}

// This is used to check that the value of new contract active UTxOs are correct. It returns the
// value of the offer asset so that it can be checked against the offer_quantity in the datum. When
// ada is the offer asset, the first Int should be offer_quantity + contract_deposit. It also checks
// that the UTxO contains the required beacons. Due to the way this function is written, beacon
// tokens do not count towards the offer quantities even if they are the offer asset. Therefore,
// beacon tokens cannot be used as the offer asset.
fn extract_offer_quantity(
  val: Value,
  active_beacon_id: ActiveBeaconId,
  offer_beacon: OfferBeacon,
  ask_beacon: AskBeacon,
  trading_pair_beacon: TradingPairBeacon,
  contract_id: ContractId,
  offer_asset: Asset
) -> (Int,Int) {
  // Extract out the offer asset info.
  let (offer_asset_id, offer_asset_name) = offer_asset

  // A function to map over the value of this UTxO. It will return the total number of the offer
  // asset and the total amount of ada.
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: (Int,Int,Bool)) {
    let (cn, tns_dict) = x
    let (offer_quantity, ada_quantity, has_proper_beacons) = acc

    // Check the beacons.
    if cn == core.from_active_beacon_id(active_beacon_id) {
      let tns_list = dict.to_list(tns_dict)

      // A function to check each beacon.
      let beacon_check = fn(y: (AssetName,Int), inner_acc: (Bool,Bool,Bool,Bool)) {
        let (tn,n) = y
        let (offer_check, ask_check, trading_check, contract_id_check) = inner_acc
        if tn == core.get_offer_beacon_name(offer_beacon) {
          // There must only be one Offer beacon. If n was zero, tn could not have matched.
          ( error_if_false(@"UTxO has too many Offer beacons", n == 1)
          , ask_check
          , trading_check
          , contract_id_check
          )
        } else if tn == core.get_ask_beacon_name(ask_beacon) {
          // There must only be one Ask beacon. If n was zero, tn could not have matched.
          ( offer_check
          , error_if_false(@"UTxO has too many Ask beacons", n == 1)
          , trading_check
          , contract_id_check
          )
        } else if tn == core.get_trading_pair_beacon_name(trading_pair_beacon) {
          // There must only be one TradingPair beacon. If n was zero, tn could not have matched.
          ( offer_check
          , ask_check
          , error_if_false(@"UTxO has too many TradingPair beacons", n == 1)
          , contract_id_check
          )
        } else if tn == core.get_contract_id_name(contract_id) {
          // There must only be one ContractId beacon. If n was zero, tn could not have matched.
          ( offer_check
          , ask_check
          , trading_check
          , error_if_false(@"UTxO has too many Contract IDs", n == 1)
          )
        } else {
          error @"UTxO has a beacon with the wrong name"
        }
      }

      when list.foldl(tns_list, (False,False,False,False), beacon_check) is {
        (True,True,True,True) -> (offer_quantity, ada_quantity, True)

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
            (offer_quantity+n, ada_quantity, has_proper_beacons)
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

      (offer_quantity, ada_quantity+n, has_proper_beacons)
    }

    else { error @"No extraneous assets allowed in the UTxO" }
  }

  // ada_quantity will be 0 when ada is the offer asset.
  let (offer_quantity, ada_quantity, has_proper_beacons) =
    list.foldl(value.to_dict(val) |> dict.to_list(_), (0,0,False), foo)

  expect True = error_if_false(
    @"UTxO is missing beacons",
    has_proper_beacons
  )

  (offer_quantity, ada_quantity)
}

// Determine what outputs and minting/burning are required based on the proposal being purchased.
fn process_purchase_input(
  address_observer_hash: ScriptHash,
  utxo_ref: OutputReference,
  input_address: Address,
  old_datum: Datum, 
  desired_terms_index: Int, 
  old_value: Value,
  acc_active_mint: Dict<AssetName,Int>,
  acc_proposal_burn: Dict<AssetName,Int>
) -> (RequiredContractOutput,RequiredPremiumOutput,Dict<AssetName,Int>,Dict<AssetName,Int>) {
  when utils.parse_datum(old_datum) is {
    // The datum must be a ProposalDatum.
    ActiveDatum{..} -> error @"Input is not a Proposal UTxO"

    ProposalDatum{
      proposal_beacon_policy_id,
      active_beacon_policy_id,
      offer_asset,
      offer_quantity,
      ask_asset,
      trading_pair_beacon,
      offer_asset_beacon,
      ask_asset_beacon,
      premium_asset,
      contract_deposit,
      payment_address,
      possible_terms,
      ..
    } -> {
      // Get the desired terms from the possible_terms list. Crash the script if the index is
      // invalid.
      let Terms{strike_price,expiration,premium} = 
        when list.at(possible_terms,desired_terms_index) is {
          None -> error @"Invalid terms index"
          Some(x) -> x
        }

      // Proposal UTxOs should have four beacons. All of them must be burned.
      let new_burn = when value.tokens(old_value,proposal_beacon_policy_id) |> dict.to_list(_) is {
        [(tn1,n1),(tn2,n2),(tn3,n3),(tn4,n4)] -> 
          acc_proposal_burn 
          |> dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn3,-n3,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn4,-n4,utils.sum_token_values,byte.compare)

        _ -> error @"Invalid Proposal UTxO found"
      }

      // A helper variable since this will be used repeatedly.
      let new_contract_id = core.gen_contract_id(utxo_ref)

      let contract_datum = ActiveDatum {
        proposal_beacon_policy_id: proposal_beacon_policy_id,
        active_beacon_policy_id: active_beacon_policy_id,
        address_observer_hash: address_observer_hash,
        offer_asset: offer_asset,
        offer_quantity: offer_quantity,
        ask_asset: ask_asset,
        trading_pair_beacon: trading_pair_beacon,
        offer_asset_beacon: offer_asset_beacon,
        ask_asset_beacon: ask_asset_beacon,
        strike_price: strike_price,
        expiration: expiration,
        contract_deposit: contract_deposit,
        payment_address: payment_address,
        contract_id: core.get_contract_id_name(new_contract_id)
      } |> InlineDatum

      let new_contract_output = RequiredContractOutput {
        active_beacon_id: core.to_active_beacon_id(active_beacon_policy_id),
        trading_pair_beacon: core.unsafe_to_trading_pair_beacon(trading_pair_beacon),
        offer_beacon: core.unsafe_to_offer_beacon(offer_asset_beacon),
        ask_beacon: core.unsafe_to_ask_beacon(ask_asset_beacon),
        contract_id: new_contract_id,
        offer_asset: offer_asset,
        offer_quantity: offer_quantity,
        contract_deposit: contract_deposit,
        req_datum: contract_datum,
        req_address: input_address
      }

      let new_premium_output = RequiredPremiumOutput {
        active_beacon_id: core.to_active_beacon_id(active_beacon_policy_id),
        contract_id: new_contract_id,
        payment_address: payment_address,
        premium_asset: premium_asset,
        premium: premium
      }

      let new_mint = acc_active_mint |>
        // Mint a TradingPair active beacon.
        dict.insert_with(_,trading_pair_beacon,1,utils.sum_token_values,byte.compare) |>
        // Mint an Offer active beacon.
        dict.insert_with(_,offer_asset_beacon,1,utils.sum_token_values,byte.compare) |>
        // Mint an Ask active beacon.
        dict.insert_with(_,ask_asset_beacon,1,utils.sum_token_values,byte.compare) |>
        // Mint two ContractIds.
        dict.insert_with(_,core.get_contract_id_name(new_contract_id),2,utils.sum_token_values,byte.compare)

      (new_contract_output, new_premium_output, new_mint, new_burn)
    }
  }
}

// Determine what outputs and burning are required based on the Active UTxO being executed.
fn process_execution_input(
  upper_bound: Option<PosixTime>,
  old_datum: Datum, 
  old_value: Value,
  acc_active_burn: Dict<AssetName,Int>
) -> (RequiredExecutionOutput,Dict<AssetName,Int>) {
  when utils.parse_datum(old_datum) is {
    // The datum must be an ActiveDatum.
    ProposalDatum{..} -> error @"Input is not an Active UTxO"

    ActiveDatum{
      active_beacon_policy_id,
      offer_quantity,
      ask_asset,
      trading_pair_beacon,
      offer_asset_beacon,
      ask_asset_beacon,
      strike_price,
      expiration,
      contract_deposit,
      payment_address,
      contract_id,
      ..
    } -> {
      // The contract's expiration must not have passed yet.
      expect True = when upper_bound is {
        None -> error "invalid-hereafter not specified"
        Some(upper) -> error_if_false(
          @"invalid-hereafter must be <= expiration",
          upper <= expiration
        )
      }

      // Active UTxOs should have four beacons. All of them must be burned. Two copies of the
      // contract_id must be burned.
      let new_burn = when value.tokens(old_value,active_beacon_policy_id) |> dict.to_list(_) is {
        [(_,_),(_,_),(_,_),(_,_)] -> 
          acc_active_burn 
          |> dict.insert_with(_,ask_asset_beacon,-1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,offer_asset_beacon,-1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,trading_pair_beacon,-1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,contract_id,-2,utils.sum_token_values,byte.compare)

        _ -> error @"Invalid Active UTxO found"
      }

      let new_execution_output = RequiredExecutionOutput {
        active_beacon_id: core.to_active_beacon_id(active_beacon_policy_id),
        contract_id: core.unsafe_to_contract_id(contract_id),
        payment_address: payment_address,
        ask_asset: ask_asset,
        strike_price: strike_price,
        offer_quantity: offer_quantity,
        contract_deposit: contract_deposit
      }

      (new_execution_output, new_burn)
    }
  }
}

// Determine what burning is required based on the Active UTxO being closed.
fn process_close_expired_input(
  lower_bound: Option<PosixTime>,
  active_beacon_id: ActiveBeaconId,
  old_datum: Datum, 
  old_value: Value,
  acc_active_burn: Dict<AssetName,Int>
) -> Dict<AssetName,Int> {
  when utils.parse_datum(old_datum) is {
    // The datum must be an ActiveDatum.
    ProposalDatum{..} -> error @"Input is not an Active UTxO"

    ActiveDatum{expiration, ..} -> {
      // Check that the contract has expired.
      expect True = when lower_bound is {
        None -> error @"invalid-before not specified"
        Some(lower) -> error_if_false(
          @"invalid-before must be >= expiration",
          expiration <= lower
        )
      }

      // Get the Active beacons in this UTxO and pattern match on it. There should be four active
      // beacons.
      when value.tokens(old_value,core.from_active_beacon_id(active_beacon_id)) |> dict.to_list() is {
        [(tn1,n1),(tn2,n2),(tn3,n3),(tn4,n4)] -> {
          acc_active_burn 
          |> dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn3,-n3,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn4,-n4,utils.sum_token_values,byte.compare)
        }

        // Ignore invalid Active UTxOs. There should never be an Active UTxO with more or less than
        // four beacons. It should either have four or none.
        _ -> acc_active_burn 
      }
    }
  }
}

// Compare the inputs to the redeemer maps to get all inputs that need to be validated. It returns
// the required information for checking the outputs as well as mint/burn. The lower_bound and
// upper_bound are passed as Option so that the script won't require them if they aren't needed.
fn get_input_info(
  lower_bound: Option<PosixTime>,
  upper_bound: Option<PosixTime>,
  proposal_beacon_policy_id: ProposalBeaconId,
  active_beacon_policy_id: ActiveBeaconId,
  options_script_credential: Credential,
  address_observer_hash: ScriptHash,
  inputs: List<Input>,
  redeemers: List<(ScriptPurpose,Redeemer)>,
  result: (
    List<RequiredContractOutput>,
    List<RequiredPremiumOutput>,
    List<RequiredExecutionOutput>,
    Dict<AssetName,Int>, // Required Active beacons to mint/burn.
    Dict<AssetName,Int>, // Required Proposal beacons to burn.
    Option<ProposalBeaconsRedeemer>
  )
) -> (
  List<RequiredContractOutput>,
  List<RequiredPremiumOutput>,
  List<RequiredExecutionOutput>,
  Dict<AssetName,Int>, // Required Active beacons to mint/burn.
  Dict<AssetName,Int>, // Required Proposal beacons to burn.
  Option<ProposalBeaconsRedeemer>
) {
  let (
    req_contract_outputs,
    req_premium_outputs,
    req_execution_outputs,
    req_active_mint,
    req_proposal_burn,
    proposal_redeemer
  ) = result

  when (inputs,redeemers) is {
    // Compare the next input to the next redeemer. They are both assumed to be in
    // lexicographical order.
    ( [ Input(in_ref,Output(Address(pay_cred,_) as addr,val,datum,_)), ..rest_ins ]
    , [ (Spend(red_ref),used_redeemer), ..rest_reds ]
    ) -> {
      // If the input is an option's script input, do all of the checks.
      if pay_cred == options_script_credential {
        // This is probably not necessary for now but there are CIPs to enable specifying different
        // orderings. This is here mostly as a reminder to consider it in the future.
        expect True = error_if_false(
          @"Inputs are not in lexicographical order",
          in_ref == red_ref
        )

        expect used_redeemer_: OptionsRedeemer = used_redeemer
        when used_redeemer_ is {
          redeemers.PurchaseContract{desired_terms_index} -> {
            let (
              new_contract_output,
              new_premium_output,
              new_active_mint,
              new_proposal_burn
            ) = process_purchase_input(
              address_observer_hash,
              in_ref,
              addr,
              datum,
              desired_terms_index,
              val,
              req_active_mint,
              req_proposal_burn
            )

            get_input_info(
              lower_bound,
              upper_bound,
              proposal_beacon_policy_id,
              active_beacon_policy_id,
              options_script_credential,
              address_observer_hash,
              rest_ins,
              rest_reds,
              ( list.concat(req_contract_outputs,[new_contract_output])
              , list.concat(req_premium_outputs,[new_premium_output])
              , req_execution_outputs
              , new_active_mint
              , new_proposal_burn
              , proposal_redeemer
              )
            )
          }

          redeemers.ExecuteContract -> {
            let (new_execution_output,new_active_mint) = process_execution_input(
              upper_bound,
              datum,
              val,
              req_active_mint
            )

            get_input_info(
              lower_bound,
              upper_bound,
              proposal_beacon_policy_id,
              active_beacon_policy_id,
              options_script_credential,
              address_observer_hash,
              rest_ins,
              rest_reds,
              ( req_contract_outputs
              , req_premium_outputs
              , list.concat(req_execution_outputs,[new_execution_output])
              , new_active_mint
              , req_proposal_burn
              , proposal_redeemer
              )
            )
          }

          redeemers.CloseExpiredContract -> {
            let new_active_mint = process_close_expired_input(
              lower_bound,
              active_beacon_policy_id,
              datum,
              val,
              req_active_mint
            )

            get_input_info(
              lower_bound,
              upper_bound,
              proposal_beacon_policy_id,
              active_beacon_policy_id,
              options_script_credential,
              address_observer_hash,
              rest_ins,
              rest_reds,
              ( req_contract_outputs
              , req_premium_outputs
              , req_execution_outputs
              , new_active_mint
              , req_proposal_burn
              , proposal_redeemer
              )
            )
          }

          // If the input is using a different options redeemer, it will be checked by the other
          // redeemer's logic. It can be safely skipped in this execution.
          _ ->
            get_input_info(
              lower_bound,
              upper_bound,
              proposal_beacon_policy_id,
              active_beacon_policy_id,
              options_script_credential,
              address_observer_hash,
              rest_ins,
              rest_reds,
              result
            )
        }

      // If it is not an options script input, check if it is a pubkey input. Pubkey inputs don't
      // use redeemers so they need to be handled differently.
      } else if utils.is_pub_key_credential(pay_cred) {
        // If the input is a pubkey input, the current spend redeemer's input must still
        // be found.
        get_input_info(
          lower_bound,
          upper_bound,
          proposal_beacon_policy_id,
          active_beacon_policy_id,
          options_script_credential,
          address_observer_hash,
          rest_ins, // Move on for inputs.
          redeemers, // Keep the current redeemer.
          result
        )

      // If none of the above branches matched, then this must be a input for another script.
      } else {
        // If the input is a script input that is not an options' input, move on for both
        // the inputs and the redeemers.
        get_input_info(
          lower_bound,
          upper_bound,
          proposal_beacon_policy_id,
          active_beacon_policy_id,
          options_script_credential,
          address_observer_hash,
          rest_ins, // Move on for inputs.
          rest_reds, // Move on for redeemers.
          result
        )
      }
    }

    // If the redeemer is not a Spend purpose, check what redeemer was used for the
    // proposal_beacon_script.
    (_ , [(purpose,used_redeemer), ..rest_reds]) -> when proposal_redeemer is {
      // The proposal_beacon_script redeemer was already found so skip this redeemer.
      Some(_) -> get_input_info(
        lower_bound,
        upper_bound,
        proposal_beacon_policy_id,
        active_beacon_policy_id,
        options_script_credential,
        address_observer_hash,
        inputs,
        rest_reds,
        result
      )

      // The proposal_beacon_script redeemer still needs to be found.
      None -> {
        // These helper variables are because the actual code is long.
        let proposal_beacon_id = core.from_proposal_beacon_id(proposal_beacon_policy_id)
        let stake_cred = Inline(ScriptCredential(proposal_beacon_id))

        // The proposal_beacon_script can be used as either a minting policy or a staking script.
        if purpose == Mint(proposal_beacon_id) || purpose == WithdrawFrom(stake_cred) {
          expect used_redeemer_: ProposalBeaconsRedeemer = used_redeemer
          get_input_info(
            lower_bound,
            upper_bound,
            proposal_beacon_policy_id,
            active_beacon_policy_id,
            options_script_credential,
            address_observer_hash,
            inputs,
            rest_reds,
            ( req_contract_outputs
            , req_premium_outputs
            , req_execution_outputs
            , req_active_mint
            , req_proposal_burn
            , Some(used_redeemer_)
            )
          )
        } else {
          // This redeemer is not for the proposal_beacon_script so skip it.
          get_input_info(
            lower_bound,
            upper_bound,
            proposal_beacon_policy_id,
            active_beacon_policy_id,
            options_script_credential,
            address_observer_hash,
            inputs,
            rest_reds,
            result
          )
        }
      }
    }

    // There are no more redeemers to check.
    _ -> result
  }
}

// This checks the required execution output to see if it has the proper value.
fn check_execution_output(
  ask_asset: Asset,
  strike_price: Fraction,
  offer_quantity: Int,
  contract_deposit: Int,
  output_value: Value
) -> Bool {
  let (strike_num,strike_den) = strike_price

  let (actual_ask_quantity,ada_quantity) = extract_ask_quantity(output_value, ask_asset)

  if ask_asset == (#"",#"") {
    offer_quantity * strike_num <= strike_den * (actual_ask_quantity - contract_deposit)
  } else { 
    and {
      offer_quantity * strike_num <= strike_den * actual_ask_quantity,
      
      // This can be larger in case more ada is needed.
      ada_quantity >= contract_deposit
    }
  }
}

// This checks the required new contract output to see if it has the proper value. This also checks
// that the output has the proper beacons.
fn check_contract_output(
  active_beacon_id: ActiveBeaconId,
  offer_beacon: OfferBeacon,
  ask_beacon: AskBeacon,
  trading_pair_beacon: TradingPairBeacon,
  contract_id: ContractId,
  offer_asset: Asset,
  offer_quantity: Int,
  contract_deposit: Int,
  output_value: Value
) -> Bool {
  // Get the amount of the offer asset and the amount of ada in the UTxO. When ada is the
  // offer asset, the ada_quantity will be unused.
  let (actual_offer_quantity,ada_quantity) = extract_offer_quantity(
    output_value,
    active_beacon_id,
    offer_beacon,
    ask_beacon,
    trading_pair_beacon,
    contract_id,
    offer_asset
  )

  if offer_asset == (#"",#"") { 
    // This can be larger in case more ada is needed.
    actual_offer_quantity >= offer_quantity + contract_deposit
  } else { 
    and {
      // This can be larger in case more ada is needed.
      ada_quantity >= contract_deposit,
      
      // There is no need to deposit more of the offer_asset
      actual_offer_quantity == offer_quantity 
    }
  }
}

// This checks the required premium output to see if it has the required premium. The contract id 
// does not go in the premium output. There is no need to check for extraneous assets since the
// buyer is already incentivized not to give the writer any extraneous assets.
fn check_premium_output(
  premium: Int,
  premium_asset: Asset,
  output_value: Value
) -> Bool {
  let (premium_policy_id, premium_name) = premium_asset

  error_if_false(
    @"Premium output missing premium",
    value.quantity_of(output_value, premium_policy_id, premium_name) >= premium
  )
}

// Go through the outputs and check that all required outputs are satisfied. The required outputs
// must be in the same order as the inputs.
fn check_outputs(
  req_contract_outputs: List<RequiredContractOutput>,
  req_premium_outputs: List<RequiredPremiumOutput>,
  req_execution_outputs: List<RequiredExecutionOutput>,
  outputs: List<Output>,
) -> Bool {
  when (req_contract_outputs, req_premium_outputs, req_execution_outputs, outputs) is {
    // Only execution payment outputs still need to be found
    ( []
    , []
    , [ execution_output, ..rest_execution_outputs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredExecutionOutput{
        active_beacon_id, 
        contract_id,
        payment_address,
        ask_asset,
        strike_price,
        offer_quantity,
        contract_deposit
      } = execution_output

      // Check if it is the next required execution output.
      if addr == payment_address && d == InlineDatum((active_beacon_id,contract_id)) {
        expect True = error_if_false(
          @"Execution payment output has wrong value",
          check_execution_output(ask_asset,strike_price,offer_quantity,contract_deposit,val)
        )

        check_outputs(
          [],
          [],
          rest_execution_outputs, // Move on to the next execution output.
          rest_outs // Move on to the next output.
        )

      // This execution output still needs to be found.
      } else {
        check_outputs(
          [],
          [],
          req_execution_outputs, // Keep the current required execution output.
          rest_outs // Move on to the next output.
        )
      }
    }

    // Only the contract outputs still need to be found.
    ( [ contract_output, ..rest_contract_outputs ]
    , []
    , []
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredContractOutput{
        active_beacon_id, 
        trading_pair_beacon,
        offer_beacon,
        ask_beacon,
        contract_id,
        offer_asset,
        offer_quantity,
        contract_deposit,
        req_datum,
        req_address
      } = contract_output

      // Check if it is the next required contract output.
      if addr == req_address && d == req_datum {
        expect True = error_if_false(
          @"Contract output has wrong value",
          check_contract_output(
            active_beacon_id,
            offer_beacon,
            ask_beacon,
            trading_pair_beacon,
            contract_id,
            offer_asset,
            offer_quantity,
            contract_deposit,
            val
          )
        )

        check_outputs(
          rest_contract_outputs, // Move on to the next contract output.
          [],
          [],
          rest_outs // Move on to the next output.
        )

      // This contract output still needs to be found.
      } else {
        check_outputs(
          req_contract_outputs, // Keep the current required contract output.
          [],
          [],
          rest_outs // Move on to the next output.
        )
      }
    }

    // Only the premium outputs still need to be found.
    ( []
    , [ premium_output, ..rest_premium_outputs ]
    , []
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredPremiumOutput{
        active_beacon_id,
        contract_id,
        payment_address,
        premium_asset,
        premium
      } = premium_output

      // Check if the output is the next required premium output.
      if addr == payment_address && d == InlineDatum((active_beacon_id,contract_id)) {
        // This has its own error messages.
        expect True = check_premium_output(premium, premium_asset, val)

        check_outputs(
          [],
          rest_premium_outputs, // Move on to the next premium output.
          [],
          rest_outs // Move on to the next output.
        )

      // This premium output still needs to be found.
      } else {
        check_outputs(
          [],
          req_premium_outputs, // Keep the current required premium output.
          [],
          rest_outs // Move on to the next output.
        )
      }
    }

    // Both contract outputs and premium outputs still need to be found. 
    ( [ contract_output, ..rest_contract_outputs ]
    , [ premium_output, ..rest_premium_outputs ]
    , []
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredContractOutput{
        active_beacon_id, 
        trading_pair_beacon,
        offer_beacon,
        ask_beacon,
        contract_id: co_contract_id, // This may be a different one than the next req. premium.
        offer_asset,
        offer_quantity,
        contract_deposit,
        req_datum,
        req_address
      } = contract_output

      let RequiredPremiumOutput{
        payment_address,
        premium_asset,
        premium,
        contract_id: p_contract_id, // This may be a different one than the next req. contract.
        ..
      } = premium_output

      // Check if it is the next required contract output.
      if addr == req_address && d == req_datum {
        expect True = error_if_false(
          @"Contract output has wrong value",
          check_contract_output(
            active_beacon_id,
            offer_beacon,
            ask_beacon,
            trading_pair_beacon,
            co_contract_id,
            offer_asset,
            offer_quantity,
            contract_deposit,
            val
          )
        )

        check_outputs(
          rest_contract_outputs, // Move on to the next contract output.
          req_premium_outputs, // Keep the current required premium output.
          [],
          rest_outs // Move on to the next output.
        )

      // Check if it is the next required premium output.
      } else if addr == payment_address && d == InlineDatum((active_beacon_id,p_contract_id)) {
        // This has its own error messages.
        expect True = check_premium_output(premium, premium_asset, val)

        check_outputs(
          req_contract_outputs, // Keep the current required contract output.
          rest_premium_outputs, // Move on to the next premium output.
          [],
          rest_outs, // Move on to the next output.
        )

      // Move on to the next output in the transaction. Both required outputs still need to
      // be found.
      } else {
        check_outputs(
          req_contract_outputs, // Keep the current required contract output.
          req_premium_outputs, // Keep the current required premium output.
          [],
          rest_outs // Move on to the next output.
        )
      }
    }

    // Both contract outputs and execution payment outputs still need to be found. 
    ( [ contract_output, ..rest_contract_outputs ]
    , []
    , [ execution_output, ..rest_execution_outputs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredContractOutput{
        active_beacon_id, 
        trading_pair_beacon,
        offer_beacon,
        ask_beacon,
        contract_id: co_contract_id,
        offer_asset,
        offer_quantity: co_offer_quantity,
        contract_deposit: co_contract_deposit,
        req_datum,
        req_address
      } = contract_output

      let RequiredExecutionOutput{
        contract_id: e_contract_id,
        payment_address,
        ask_asset,
        strike_price,
        offer_quantity: e_offer_quantity,
        contract_deposit: e_contract_deposit,
        ..
      } = execution_output

      // Check if it is the next required contract output.
      if addr == req_address && d == req_datum {
        expect True = error_if_false(
          @"Contract output has wrong value",
          check_contract_output(
            active_beacon_id,
            offer_beacon,
            ask_beacon,
            trading_pair_beacon,
            co_contract_id,
            offer_asset,
            co_offer_quantity,
            co_contract_deposit,
            val
          )
        )

        check_outputs(
          rest_contract_outputs, // Move on to the next contract output.
          [],
          req_execution_outputs, // Keep the current required execution output.
          rest_outs // Move on to the next output.
        )

      // Check if it is the next required execution output.
      } else if addr == payment_address && d == InlineDatum((active_beacon_id,e_contract_id)) {
        expect True = error_if_false(
          @"Execution payment output has wrong value",
          check_execution_output(ask_asset,strike_price,e_offer_quantity,e_contract_deposit,val)
        )

        check_outputs(
          req_contract_outputs, // This contract output still needs to be found.
          [],
          rest_execution_outputs, // Move on to the next execution output.
          rest_outs // Move on to the next output.
        )

      // Both outputs still needs to be found.
      } else {
        check_outputs(
          req_contract_outputs, // Keep the current required contract output.
          [],
          req_execution_outputs, // Keep the current required execution payment output.
          rest_outs // Move on to the next output.
        )
      }
    }

    // Both premium outputs and execution payment outputs still need to be found. 
    ( []
    , [ premium_output, ..rest_premium_outputs ]
    , [ execution_output, ..rest_execution_outputs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {

      let RequiredPremiumOutput{
        active_beacon_id,
        payment_address: p_payment_address,
        premium_asset,
        premium,
        contract_id: p_contract_id, // This may be a different one than the next req. contract.
      } = premium_output

      let RequiredExecutionOutput{
        contract_id: e_contract_id,
        payment_address: e_payment_address,
        ask_asset,
        strike_price,
        offer_quantity,
        contract_deposit,
        ..
      } = execution_output

      // Check if it is the next required premium output.
      if addr == p_payment_address && d == InlineDatum((active_beacon_id,p_contract_id)) {
        // This has its own error messages.
        expect True = check_premium_output(premium, premium_asset, val)

        check_outputs(
          [],
          rest_premium_outputs, // Move on to the next premium output.
          req_execution_outputs, // This execution payment output still needs to be found.
          rest_outs // Move on to the next output.
        )

      // Check if it is the next required execution output.
      } else if addr == e_payment_address && d == InlineDatum((active_beacon_id,e_contract_id)) {
        expect True = error_if_false(
          @"Execution payment output has wrong value",
          check_execution_output(ask_asset,strike_price,offer_quantity,contract_deposit,val)
        )

        check_outputs(
          [],
          req_premium_outputs, // Keep the current required premium output.
          rest_execution_outputs, // Move on to the next execution output.
          rest_outs // Move on to the next output.
        )

      // Both outputs still needs to be found.
      } else {
        check_outputs(
          [],
          req_premium_outputs, // Keep the current required premium output.
          req_execution_outputs, // Keep the current required execution output.
          rest_outs // Move on to the next output.
        )
      }
    }

    // All outputs still need to be found. 
    ( [ contract_output, ..rest_contract_outputs ]
    , [ premium_output, ..rest_premium_outputs ]
    , [ execution_output, ..rest_execution_outputs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredContractOutput{
        active_beacon_id, 
        trading_pair_beacon,
        offer_beacon,
        ask_beacon,
        contract_id: co_contract_id, // This may be a different one than the next req. premium.
        offer_asset,
        offer_quantity: co_offer_quantity,
        contract_deposit: co_contract_deposit,
        req_datum,
        req_address
      } = contract_output

      let RequiredPremiumOutput{
        payment_address: p_payment_address,
        premium_asset,
        premium,
        contract_id: p_contract_id, // This may be a different one than the next req. contract.
        ..
      } = premium_output

      let RequiredExecutionOutput{
        contract_id: e_contract_id,
        payment_address: e_payment_address,
        ask_asset,
        strike_price,
        offer_quantity: e_offer_quantity,
        contract_deposit: e_contract_deposit,
        ..
      } = execution_output

      // Check if it is the next required contract output.
      if addr == req_address && d == req_datum {
        expect True = error_if_false(
          @"Contract output has wrong value",
          check_contract_output(
            active_beacon_id,
            offer_beacon,
            ask_beacon,
            trading_pair_beacon,
            co_contract_id,
            offer_asset,
            co_offer_quantity,
            co_contract_deposit,
            val
          )
        )

        check_outputs(
          rest_contract_outputs, // Move on to the next contract output.
          req_premium_outputs, // Keep the current required premium output.
          req_execution_outputs, // Keep the current required execution output.
          rest_outs // Move on to the next output.
        )

      // Check if it is the next required premium output.
      } else if addr == p_payment_address && d == InlineDatum((active_beacon_id,p_contract_id)) {
        // This has its own error messages.
        expect True = check_premium_output(premium, premium_asset, val)

        check_outputs(
          req_contract_outputs, // Keep the current required contract output.
          rest_premium_outputs, // Move on to the next premium output.
          req_execution_outputs, // Keep the current required execution output.
          rest_outs // Move on to the next output.
        )

      // Check if it is the next required execution output.
      } else if addr == e_payment_address && d == InlineDatum((active_beacon_id,e_contract_id)) {
        expect True = error_if_false(
          @"Execution payment output has wrong value",
          check_execution_output(ask_asset,strike_price,e_offer_quantity,e_contract_deposit,val)
        )

        check_outputs(
          req_contract_outputs, // Keep the current required contract output.
          req_premium_outputs, // Keep the current required premium output.
          rest_execution_outputs, // Move on to the next execution output.
          rest_outs // Move on to the next output.
        )

      // All outputs still needs to be found.
      } else {
        check_outputs(
          req_contract_outputs, // Keep the current required contract output.
          req_premium_outputs, // Keep the current required premium output.
          req_execution_outputs, // Keep the current required execution payment output.
          rest_outs // Move on to the next output.
        )
      }
    }

    ([], [], [], _) -> True

    ([_,..], _, _, []) -> error "Not all required contract outputs found"

    (_, [_,..], _, []) -> error "Not all required premium outputs found"

    (_, _, [_,..], []) -> error "Not all required execution payment outputs found"
  }
}

// Check all inputs that should compose.
pub fn valid_purchase_execute_or_close_expired(
  inputs: List<Input>,
  outputs: List<Output>,
  redeemers: Dict<ScriptPurpose,Redeemer>,
  mint: Value,
  options_script_credential: Credential,
  address_observer_hash: ScriptHash,
  proposal_beacon_policy_id: ProposalBeaconId,
  active_beacon_policy_id: ActiveBeaconId,
  validity_range: ValidityRange
) -> Bool {
  let (
    req_contract_outputs,
    req_premium_outputs,
    req_execution_outputs,
    req_active_mint,
    req_proposal_burn,
    proposal_redeemer
  ) = get_input_info(
    utils.get_lower_bound_optional(validity_range),
    utils.get_upper_bound_optional(validity_range),
    proposal_beacon_policy_id,
    active_beacon_policy_id,
    options_script_credential,
    address_observer_hash,
    inputs,
    dict.to_list(redeemers),
    ([],[],[],dict.new(),dict.new(),None)
  )

  let (actual_proposal_burn,actual_active_mint) = get_beacon_mint_and_burn(
    proposal_beacon_policy_id,
    active_beacon_policy_id,
    (None,None),
    value.to_dict(mint) |> dict.to_list(_)
  )

  // This only needs to be checked if there are purchases in this transaction.
  expect True = when req_contract_outputs is {
    [] -> True
    _ -> {
      // The proposal_beacon_script must be executed using CreateCloseOrUpdateProposals or
      // BurnProposalBeacons. If the latter is used, the proper proposal beacons must be burned.
      when proposal_redeemer is {
        // The proposal_beacon_script will ensure none of the beacons are misused.
        Some(redeemers.CreateCloseOrUpdateProposals) -> True 

        Some(redeemers.BurnProposalBeacons) -> error_if_false(
          @"The wrong proposal beacons were burned", 
          actual_proposal_burn == Some(req_proposal_burn)
        )

        _ -> error @"Proposal beacon script not executed with proper redeemer"
      }
    }
  }

  expect True = error_if_false(
    @"The wrong active beacons were minted/burned", 
    actual_active_mint == Some(
      // Since it is possible for the calculated mint to contain zero values while it is not
      // possible for the actual mint to contain zero values, the zero values must be filtered out
      // of the calculated mint.
      dict.filter(req_active_mint, fn(_k,v) { v != 0 })
    )
  )

  // This either returns True or fails with an error.
  check_outputs(req_contract_outputs, req_premium_outputs, req_execution_outputs, outputs)
}
