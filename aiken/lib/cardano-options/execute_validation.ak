use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/list
use aiken/time.{PosixTime}
use aiken/transaction.
  { Datum
  , InlineDatum
  , Output
  , Input
  , ScriptPurpose
  , Redeemer
  , Spend
  } as tx
use aiken/transaction/credential.{Address, ScriptCredential}
use aiken/transaction/value.{Value, PolicyId, AssetName}

use cardano_options/data/core.{ActiveBeaconId,Fraction,ContractId,Asset,ScriptHash}
use cardano_options/data/datums.{ActiveDatum}
use cardano_options/data/redeemers.{OptionsRedeemer,ExecuteContract}
use cardano_options/utils.{trace_if_false, error_if_false}

type RequiredAskOutput {
  active_beacon_id: ActiveBeaconId,
  contract_id: ContractId,
  payment_address: Address,
  ask_asset: Asset,
  strike_price: Fraction,
  offer_quantity: Int,
  contract_deposit: Int
}

// This is used to check that the value of final payment UTxOs are correct. It returns the value of
// the ask asset so that it can be checked against the ask_quantity in the datum. When ada is
// the ask asset, the first Int should be ask_quantity + contract_deposit.
fn extract_payment_quantity(
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

// Determine what outputs are required based on the input.
fn process_input(
  upper_bound: PosixTime,
  old_datum: Datum, 
  old_value: Value,
  acc_burn: Dict<AssetName,Int>
) -> (RequiredAskOutput,Dict<AssetName,Int>) {
  when utils.parse_datum(old_datum) is {
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
      expect True = error_if_false(
        @"Contract is already expired",
        upper_bound <= expiration
      )

      // Proposal UTxOs should have four beacons. All of them must be burned.
      let new_burn = when value.tokens(old_value,active_beacon_policy_id) |> dict.to_list(_) is {
        [(_,_),(_,_),(_,_),(_,_)] -> 
          acc_burn 
          |> dict.insert_with(_,ask_asset_beacon,-1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,offer_asset_beacon,-1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,trading_pair_beacon,-1,utils.sum_token_values,byte.compare)
          |> dict.insert_with(_,contract_id,-2,utils.sum_token_values,byte.compare)

        _ -> error @"Invalid Active UTxO found"
      }

      let new_ask_output = RequiredAskOutput {
        active_beacon_id: core.to_active_beacon_id(active_beacon_policy_id),
        contract_id: core.unsafe_to_contract_id(contract_id),
        payment_address: payment_address,
        ask_asset: ask_asset,
        strike_price: strike_price,
        offer_quantity: offer_quantity,
        contract_deposit: contract_deposit
      }

      (new_ask_output,new_burn)
    }

    _ -> error @"Input is not an Active UTxO"
  }
}

// Compare the inputs to the redeemer maps to get all inputs that are for executions.
// It returns the required information for checking the outputs as well as mint/burn.
fn get_input_info(
  options_script_hash: ScriptHash,
  upper_bound: PosixTime,
  inputs: List<Input>,
  redeemers: List<(ScriptPurpose,Redeemer)>,
  result: (List<RequiredAskOutput>, Dict<AssetName,Int>)
) -> (List<RequiredAskOutput>, Dict<AssetName,Int>) {
  let (req_ask_outputs,req_burn) = result

  when (inputs,redeemers) is {
    // Compare the next input to the next redeemer. They are both assumed to be in
    // lexicographical order.
    ( [ Input(in_ref,Output(Address(pay_cred,_),val,datum,_)), ..rest_ins ]
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
          // This execution only cares about inputs using the ExecuteContract redeemer.
          ExecuteContract -> {
            let (new_ask_output,new_burn) = process_input(
              upper_bound,
              datum,
              val,
              req_burn
            )

            get_input_info(
              options_script_hash,
              upper_bound,
              rest_ins,
              rest_reds,
              ( list.concat(req_ask_outputs,[new_ask_output])
              , new_burn
              )
            )
          }

          // If the input is using a different options redeemer, it will be checked by the other
          // redeemer's logic. It can be safely skipped in this execution.
          _ -> get_input_info(
            options_script_hash,
            upper_bound,
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
          options_script_hash,
          upper_bound,
          rest_ins, // Move on for inputs.
          redeemers, // Keep the current redeemer.
          result
        )

      // If none of the above branches matched, then this must be a input for another script.
      } else {
        // If the input is a script input that is not an options' input, move on for both
        // the inputs and the redeemers.
        get_input_info(
          options_script_hash,
          upper_bound,
          rest_ins, // Move on for inputs.
          rest_reds, // Move on for redeemers.
          result
        )
      }
    }

    // If the redeemer is not a Spend purpose, it can be skipped.
    (_ , [_, ..rest_reds]) -> get_input_info(
      options_script_hash,
      upper_bound,
      inputs,
      rest_reds,
      result
    )

    // There are no more redeemers to check.
    _ -> result
  }
}

// Go through the outputs and check that all required outputs are satisfied. The ask payment outputs
// are assumed to be in the order of the inputs. This function can never actually return False; it
// will crash with an error at the first failure.
fn check_outputs(
  req_ask_outputs: List<RequiredAskOutput>,
  outputs: List<Output>,
) -> Bool {
  when (req_ask_outputs,outputs) is {
    ( [ ask_output, ..rest_ask_outputs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredAskOutput{
        active_beacon_id, 
        contract_id,
        payment_address,
        ask_asset: (ask_policy_id,_) as ask_asset,
        strike_price: (strike_num,strike_den),
        offer_quantity,
        contract_deposit
      } = ask_output

      if addr == payment_address && d == InlineDatum((active_beacon_id,contract_id)) {
        let (actual_ask_quantity,ada_quantity) = extract_payment_quantity(
          val,
          ask_asset
        )

        // This will either crash or return true.
        expect True = if ask_policy_id == #"" {
          offer_quantity * strike_num <= strike_den * (actual_ask_quantity - contract_deposit)
        } else { 
          offer_quantity * strike_num <= strike_den * actual_ask_quantity && 
          ada_quantity >= contract_deposit
        }

        check_outputs(
          rest_ask_outputs, // Move on to the next ask output.
          rest_outs // Move on to the next output.
        )

      // This ask output still needs to be found.
      } else {
        check_outputs(
          req_ask_outputs, // Keep the current required ask output.
          rest_outs // Move on to the next output.
        )
      }
    }

    ([], _) -> True

    ([_,..], []) -> error "Not all required ask payment outputs found"
  }
}

pub fn valid_execution(
  options_script_hash: ScriptHash,
  upper_bound: PosixTime,
  inputs: List<Input>,
  outputs: List<Output>,
  redeemers: Dict<ScriptPurpose,Redeemer>,
  beacons_burned: Dict<AssetName,Int>
) -> Bool {
  let (req_ask_outputs, req_burn) = get_input_info(
    options_script_hash,
    upper_bound,
    inputs,
    dict.to_list(redeemers),
    ([],dict.new())
  )

  check_outputs(req_ask_outputs, outputs) && trace_if_false(
    @"Incorrect Active beacons burned",
    beacons_burned == req_burn
  )
}
