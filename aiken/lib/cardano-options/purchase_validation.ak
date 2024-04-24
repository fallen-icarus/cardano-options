use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/list
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
  } as tx
use aiken/transaction/credential.{Address, ScriptCredential, Inline}
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
  }
use cardano_options/data/datums.{ProposalDatum,ActiveDatum}
use cardano_options/data/redeemers.{OptionsRedeemer,PurchaseContract,ProposalBeaconsRedeemer}
use cardano_options/utils.{error_if_false}

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

type RequiredPremiumOutput {
  active_beacon_id: ActiveBeaconId,
  contract_id: ContractId,
  payment_address: Address,
  premium_asset: Asset,
  premium: Int
}

// This is used to check that the value of Active UTxOs are correct. It returns the value of the
// offer asset so that it can be checked against the offer_quantity in the datum. When ada is the
// offer asset, the first Int should be offer_quantity + contract_deposit. Due to the way this
// function is written, beacon tokens do not count towards the offer quantities even if they
// are the offer asset. Therefore, beacon tokens cannot be used as the offer asset.
fn extract_proposal_quantity(
  val: Value,
  active_beacon_id: ActiveBeaconId,
  offer_beacon: OfferBeacon,
  ask_beacon: AskBeacon,
  trading_pair_beacon: TradingPairBeacon,
  contract_id: ContractId,
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
    if cn == core.from_active_beacon_id(active_beacon_id) {
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

// Determine what outputs are required based on the input.
fn process_input(
  address_observer_hash: ScriptHash,
  utxo_ref: OutputReference,
  input_address: Address,
  old_datum: Datum, 
  desired_terms_index: Int, 
  old_value: Value,
  acc_mint: Dict<AssetName,Int>,
  acc_burn: Dict<AssetName,Int>
) -> (RequiredContractOutput,RequiredPremiumOutput,Dict<AssetName,Int>,Dict<AssetName,Int>) {
  when utils.parse_datum(old_datum) is {
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
      let Terms{strike_price,expiration,premium} = 
        when list.at(possible_terms,desired_terms_index) is {
          None -> error @"Invalid terms index"
          Some(x) -> x
        }

      // Proposal UTxOs should have four beacons. All of them must be burned.
      let new_burn = when value.tokens(old_value,proposal_beacon_policy_id) |> dict.to_list(_) is {
        [(tn1,n1),(tn2,n2),(tn3,n3),(tn4,n4)] -> 
          acc_burn 
          |> dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn3,-n3,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,tn4,-n4,utils.sum_token_values,byte.compare)

        _ -> error @"Invalid Proposal UTxO found"
      }

      let new_contract_id = core.gen_contract_id(utxo_ref)

      let contract_datum = ActiveDatum {
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

      let new_mint = acc_mint |>
        // Mint a TradingPair active beacon.
        dict.insert_with(_,trading_pair_beacon,1,utils.sum_token_values,byte.compare) |>
        // Mint an Offer active beacon.
        dict.insert_with(_,offer_asset_beacon,1,utils.sum_token_values,byte.compare) |>
        // Mint an Ask active beacon.
        dict.insert_with(_,ask_asset_beacon,1,utils.sum_token_values,byte.compare) |>
        // Mint two ContractIds.
        dict.insert_with(_,core.get_contract_id_name(new_contract_id),2,utils.sum_token_values,byte.compare)

      (new_contract_output,new_premium_output,new_mint,new_burn)
    }

    _ -> error @"Input is not a Proposal UTxO"
  }
}

// Compare the inputs to the redeemer maps to get all inputs that are for purchases.
// It returns the required information for checking the outputs as well as mint/burn.
fn get_input_info(
  proposal_beacon_policy_id: PolicyId,
  address_observer_hash: ScriptHash,
  options_script_hash: ScriptHash,
  inputs: List<Input>,
  redeemers: List<(ScriptPurpose,Redeemer)>,
  result: (
    List<RequiredContractOutput>,
    List<RequiredPremiumOutput>,
    Dict<AssetName,Int>,
    Dict<AssetName,Int>,
    Option<ProposalBeaconsRedeemer>
  )
) -> (
  List<RequiredContractOutput>,
  List<RequiredPremiumOutput>,
  Dict<AssetName,Int>,
  Dict<AssetName,Int>,
  Option<ProposalBeaconsRedeemer>
) {
  let (req_contract_outputs,req_premium_outputs,req_mint,req_burn,proposal_redeemer) = result

  when (inputs,redeemers) is {
    // Compare the next input to the next redeemer. They are both assumed to be in
    // lexicographical order.
    ( [ Input(in_ref,Output(Address(pay_cred,_) as addr,val,datum,_)), ..rest_ins ]
    , [ (Spend(red_ref),used_redeemer), ..rest_reds ]
    ) -> {
      // If the input is an option's script input, do all of the checks.
      if pay_cred == ScriptCredential(options_script_hash) {
        expect True = error_if_false(
          @"Inputs are not in lexicographical order",
          in_ref == red_ref
        )

        expect used_redeemer_: OptionsRedeemer = used_redeemer
        when used_redeemer_ is {
          // This execution only cares about inputs using the PurchaseContract redeemer.
          PurchaseContract{desired_terms_index} -> {
            let (new_contract_output,new_premium_output,new_mint,new_burn) = process_input(
              address_observer_hash,
              in_ref,
              addr,
              datum,
              desired_terms_index,
              val,
              req_mint,
              req_burn
            )

            get_input_info(
              proposal_beacon_policy_id,
              address_observer_hash,
              options_script_hash,
              rest_ins,
              rest_reds,
              ( list.concat(req_contract_outputs,[new_contract_output])
              , list.concat(req_premium_outputs,[new_premium_output])
              , new_mint
              , new_burn
              , proposal_redeemer
              )
            )
          }

          // If the input is using a different options redeemer, it will be checked by the other
          // redeemer's logic. It can be safely skipped in this execution.
          _ ->
            get_input_info(
              proposal_beacon_policy_id,
              address_observer_hash,
              options_script_hash,
              rest_ins,
              rest_reds,
              result
            )
        }

      // If it is not an options script input, check if it is a pubkey input.
      } else if utils.is_pub_key_credential(pay_cred) {
        // If the input is a pubkey input, the current spend redeemer's input must still
        // be found.
        get_input_info(
          proposal_beacon_policy_id,
          address_observer_hash,
          options_script_hash,
          rest_ins, // Move on for inputs.
          redeemers, // Keep the current redeemer.
          result
        )

      // If none of the above branches matched, then this must be a input for another script.
      } else {
        // If the input is a script input that is not an options' input, move on for both
        // the inputs and the redeemers.
        get_input_info(
          proposal_beacon_policy_id,
          address_observer_hash,
          options_script_hash,
          rest_ins, // Move on for inputs.
          rest_reds, // Move on for redeemers.
          result
        )
      }
    }

    // If the redeemer is not a Spend purpose, check what redeemer was used for the
    // proposal_beacon_script.
    (_ , [(purpose,used_redeemer), ..rest_reds]) -> when proposal_redeemer is {
      Some(_) -> 
        // The redeemer was already found so skip it.
        get_input_info(
          proposal_beacon_policy_id,
          address_observer_hash,
          options_script_hash,
          inputs,
          rest_reds,
          result
        )
      None -> {
        let stake_cred = Inline(ScriptCredential(proposal_beacon_policy_id))
        if purpose == Mint(proposal_beacon_policy_id) || purpose == WithdrawFrom(stake_cred) {
          expect used_redeemer_: ProposalBeaconsRedeemer = used_redeemer
          // The redeemer was already found so skip it.
          get_input_info(
            proposal_beacon_policy_id,
            address_observer_hash,
            options_script_hash,
            inputs,
            rest_reds,
            (req_contract_outputs,req_premium_outputs,req_mint,req_burn,Some(used_redeemer_))
          )
        } else {
          // This redeemer is not for the proposal_beacon_script so skip it.
          get_input_info(
            proposal_beacon_policy_id,
            address_observer_hash,
            options_script_hash,
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

// Go through the outputs and check that all required outputs are satisfied. The contract outputs
// are assumed to be in the order of the inputs. The next required contract output and the next
// required premium output do not need to be next to each other. The only requirement is that all
// contract outputs are in the order of the inputs and all premium outputs are in the order of the
// inputs. Any combination that satisfies this requirement is valid. For example, all contract
// outputs can appear first, followed by all of the premium outputs. This function can never
// actually return False; it will crash with an error at the first failure.
fn check_outputs(
  req_contract_outputs: List<RequiredContractOutput>,
  req_premium_outputs: List<RequiredPremiumOutput>,
  outputs: List<Output>,
) -> Bool {
  when (req_contract_outputs,req_premium_outputs,outputs) is {
    // Only the contract outputs still need to be found.
    ( [ contract_output, ..rest_contract_outputs ]
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

      if addr == req_address && d == req_datum {
        // Get the amount of the offer asset and the amount of ada in the UTxO. When ada is the
        // offer asset, the ada_quantity will be unused.
        let (actual_offer_quantity,ada_quantity) = extract_proposal_quantity(
          val,
          active_beacon_id,
          offer_beacon,
          ask_beacon,
          trading_pair_beacon,
          contract_id,
          offer_asset
        )

        expect True = error_if_false(
          @"UTxO has wrong value",
          if offer_asset == (#"",#"") { 
            actual_offer_quantity == offer_quantity + contract_deposit
          } else { 
            ada_quantity == contract_deposit && actual_offer_quantity == offer_quantity
          }
        )

        check_outputs(
          rest_contract_outputs, // Move on to the next contract output.
          [],
          rest_outs // Move on to the next output.
        )

      // This contract output still needs to be found.
      } else {
        check_outputs(
          req_contract_outputs, // Keep the current required contract output.
          [],
          rest_outs // Move on to the next output.
        )
      }
    }

    // Only the premium outputs still need to be found.
    ( []
    , [ premium_output, ..rest_premium_outputs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredPremiumOutput{
        active_beacon_id,
        contract_id,
        payment_address,
        premium_asset: (premium_policy_id,premium_name),
        premium
      } = premium_output

      // Check if the output is the next required premium output.
      if addr == payment_address && d == InlineDatum((active_beacon_id,contract_id)) {
        // This will either crash or return true.
        expect True = premium <= value.quantity_of(
          val,
          premium_policy_id,
          premium_name,
        )

        check_outputs(
          [],
          rest_premium_outputs, // Move on to the next premium output.
          rest_outs // Move on to the next output.
        )

      // This premium output still needs to be found.
      } else {
        check_outputs(
          [],
          req_premium_outputs, // Keep the current required premium output.
          rest_outs // Move on to the next output.
        )
      }
    }

    // Both contract outputs and premium outputs still need to be found. The next ones
    // to find are not necessarily for the same Proposal UTxO.
    ( [ contract_output, ..rest_contract_outputs ]
    , [ premium_output, ..rest_premium_outputs ]
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
        premium_asset: (premium_policy_id,premium_name),
        premium,
        contract_id: p_contract_id, // This may be a different one than the next req. contract.
        ..
      } = premium_output

      // Check if it is the next required contract output.
      if addr == req_address && d == req_datum {
        // Get the amount of the offer asset and the amount of ada in the UTxO. When ada is the
        // offer asset, the ada_quantity will be unused.
        let (actual_offer_quantity,ada_quantity) = extract_proposal_quantity(
          val,
          active_beacon_id,
          offer_beacon,
          ask_beacon,
          trading_pair_beacon,
          co_contract_id,
          offer_asset
        )

        expect True = error_if_false(
          @"UTxO has wrong value",
          if offer_asset == (#"",#"") { 
            actual_offer_quantity == offer_quantity + contract_deposit
          } else { 
            ada_quantity == contract_deposit && actual_offer_quantity == offer_quantity
          }
        )

        check_outputs(
          rest_contract_outputs, // Move on to the next contract output.
          req_premium_outputs, // This premium output still needs to be found.
          rest_outs // Move on to the next output.
        )

      // Check if it is the next required premium output.
      } else if addr == payment_address && d == InlineDatum((active_beacon_id,p_contract_id)) {
        // This will either crash or return true.
        expect True = premium <= value.quantity_of(
          val,
          premium_policy_id,
          premium_name,
        )

        check_outputs(
          req_contract_outputs, // This contract output still needs to be found.
          rest_premium_outputs, // Move on to the next premium output.
          rest_outs, // Move on to the next output.
        )

      // Move on to the next output in the transaction. Both required outputs still need to
      // be found.
      } else {
        check_outputs(
          req_contract_outputs, // Keep the current required contract output.
          req_premium_outputs, // Keep the current required premium output.
          rest_outs // Move on to the next output.
        )
      }
    }

    ([], [], _) -> True

    ([_,..], _, []) -> error "Not all required contract outputs found"

    (_, [_,..], []) -> error "Not all required premium outputs found"
  }
}

// Check all purchases.
pub fn valid_purchase(
  inputs: List<Input>,
  outputs: List<Output>,
  redeemers: Dict<ScriptPurpose,Redeemer>,
  mint: Value,
  options_script_hash: ScriptHash,
  address_observer_hash: ScriptHash,
  proposal_beacon_policy_id: ProposalBeaconId,
  active_beacon_policy_id: ActiveBeaconId
) -> Bool {
  let (
    req_contract_outputs,
    req_premium_outputs,
    req_mint,
    req_burn,
    proposal_redeemer
  ) = get_input_info(
    core.from_proposal_beacon_id(proposal_beacon_policy_id),
    address_observer_hash,
    options_script_hash,
    inputs,
    dict.to_list(redeemers),
    ([],[],dict.new(),dict.new(),None)
  )

  let (actual_proposal_burn,actual_active_mint) = get_beacon_mint_and_burn(
    proposal_beacon_policy_id,
    active_beacon_policy_id,
    (None,None),
    value.to_dict(mint) |> dict.to_list(_)
  )

  // The proposal_beacon_script must be executed using CreateCloseOrUpdate or BurnProposalBeacons.
  // If the latter is used, the proper proposal beacons must be burned.
  expect True = when proposal_redeemer is {
    // The proposal_beacon_script will ensure none of the beacons are misused.
    Some(redeemers.CreateCloseOrUpdateProposals) -> True 

    Some(redeemers.BurnProposalBeacons) -> error_if_false(
      @"The wrong proposal beacons were burned", 
      actual_proposal_burn == Some(req_burn)
    )

    _ -> error @"Proposal beacon script executed with the wrong redeemer"
  }

  expect True = error_if_false(
    @"The wrong active beacons were minted", 
    actual_active_mint == Some(req_mint)
  )

  check_outputs(req_contract_outputs, req_premium_outputs, outputs)
}
