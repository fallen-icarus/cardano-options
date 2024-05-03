use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/list
use aiken/transaction.
  { Datum
  , InlineDatum
  , Output
  , Input
  , ScriptPurpose
  , Redeemer
  , Spend
  } as tx
use aiken/transaction/credential.{Address, Credential}
use aiken/transaction/value.{Value, PolicyId, AssetName}

use cardano_options/data/core.{ScriptHash}
use cardano_options/data/datums.{ActiveDatum,ProposalDatum}
use cardano_options/data/redeemers.{OptionsRedeemer,UpdatePaymentAddress}
use cardano_options/utils.{error_if_false}

type RequiredContractOutput {
  req_value: List<(PolicyId,Dict<AssetName,Int>)>,
  req_datum: Datum,
  writer_addr: Address
}

// Add the deposit increase to the ada balance, and preserve all of the other balances.
fn add_deposit_value(
  deposit_amount: Int,
  old_value: Value,
) -> List<(PolicyId,Dict<AssetName,Int>)> {
  // A function to map over the value of this UTxO. 
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: List<(PolicyId,Dict<AssetName,Int>)>) {
    let (cn,tns_dict) = x

    if cn == #"" {
      list.concat(
        acc,
        [(cn,dict.insert_with(tns_dict, #"", deposit_amount, utils.sum_token_values, byte.compare))]
      )
    } else {
      list.concat(acc,[(cn,tns_dict)])
    }
  }

  list.foldl(value.to_dict(old_value) |> dict.to_list(_), [], foo)
}

// Get the required output information based on the input information.
fn process_input(
  writer_addr: Address,
  old_datum: Datum, 
  proxy_hash: ScriptHash,
  new_address: Address,
  deposit_amount: Int, 
  old_value: Value
) -> RequiredContractOutput {
  when utils.parse_datum(old_datum) is {
    // Datum must be an ActiveDatum.
    ProposalDatum{..} -> error @"Input is not an Active UTxO"

    ActiveDatum{
      proposal_beacon_policy_id,
      active_beacon_policy_id,
      address_observer_hash,
      offer_asset,
      offer_quantity,
      ask_asset,
      trading_pair_beacon,
      offer_asset_beacon,
      ask_asset_beacon,
      strike_price,
      expiration,
      contract_deposit,
      contract_id,
      ..
    } -> {
      expect True = error_if_false(
        @"Deposit increase not >= 0",
        deposit_amount >= 0
      )

      expect True = error_if_false(
        @"Invalid new payment address",
        utils.valid_payment_address(proxy_hash,new_address)
      )

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
        // The writer has now increased the deposit.
        contract_deposit: contract_deposit+deposit_amount,
        payment_address: new_address,
        contract_id: contract_id
      } |> InlineDatum

      RequiredContractOutput( 
        add_deposit_value(deposit_amount, old_value),
        contract_datum,
        writer_addr
      )
    }
  }
}

// Compare the inputs to the redeemer maps to get all inputs that are for address updates.
// It returns the required information for checking the outputs. 
fn get_input_info(
  options_script_credential: Credential,
  proxy_hash: ScriptHash,
  inputs: List<Input>,
  redeemers: List<(ScriptPurpose,Redeemer)>,
  req_outputs: List<RequiredContractOutput>
) -> List<RequiredContractOutput> {
  when (inputs,redeemers) is {
    // Compare the next input to the next redeemer. They are both assumed to be in
    // lexicographical order.
    ( [ Input(in_ref,Output(Address(pay_cred,_) as addr,val,datum,_)), ..rest_ins ]
    , [ (Spend(red_ref),used_redeemer), ..rest_reds ]
    ) -> {
      // If the input is a options script input, do all of the checks.
      if pay_cred == options_script_credential {
        expect True = error_if_false(
          @"Inputs are not in lexicographical order",
          in_ref == red_ref
        )

        expect used_redeemer_: OptionsRedeemer = used_redeemer
        when used_redeemer_ is {
          // This execution only cares about inputs using the UpdatePaymentAddress redeemer.
          UpdatePaymentAddress{deposit_increase,new_address} -> {
            let new_contract_output = process_input(
              addr,
              datum,
              proxy_hash,
              new_address,
              deposit_increase,
              val
            )

            get_input_info(
              options_script_credential,
              proxy_hash,
              rest_ins,
              rest_reds,
              list.concat(req_outputs, [new_contract_output])
            )
          }

          // If the input is using a different options redeemer, it will be checked by the other
          // redeemer's logic. It can be safely skipped in this execution.
          _ -> get_input_info(
            options_script_credential,
            proxy_hash,
            rest_ins,
            rest_reds,
            req_outputs
          )
        }

      // If it is not an options script input, check if it is a pubkey input.
      } else if utils.is_pub_key_credential(pay_cred) {
        // If the input is a pubkey input, the current spend redeemer's input must still
        // be found.
        get_input_info(
          options_script_credential,
          proxy_hash,
          rest_ins, // Move on for inputs.
          redeemers, // Keep the current redeemer.
          req_outputs
        )

      // If none of the above branches matched, then this must be a input for another script.
      } else {
        // If the input is a script input that is not a options' input, move on for both
        // the inputs and the redeemers.
        get_input_info(
          options_script_credential,
          proxy_hash,
          rest_ins, // Move on for inputs.
          rest_reds, // Move on for redeemers.
          req_outputs
        )
      }
    }

    // If the redeemer is not a Spend purpose, skip it.
    (_ , [_, ..rest_reds]) -> get_input_info(
      options_script_credential,
      proxy_hash,
      inputs,
      rest_reds,
      req_outputs
    )

    _ -> req_outputs
  }
}

// The outputs are also assumed to be in the same order as the inputs (ie, the outputs for options
// input 1 are before the outputs for options input 2). The script will fail if this assumption is not
// true. This assumption dramatically improves performance. This function will crash with an error 
// if any checks fail. 
fn check_outputs(
  req_outputs: List<RequiredContractOutput>,
  outputs: List<Output>,
) -> Bool {
  when (req_outputs,outputs) is {
    // The contract output still needs to be found for this options input.
    ( [contract_output, ..rest_contract_outs]
    , [Output(addr,val,d,_), ..rest_outs]
    ) -> {
      let RequiredContractOutput{
        req_value,
        req_datum,
        writer_addr
      } = contract_output

      // Check if this output is the next required contract output.
      if addr == writer_addr && d == req_datum {
        expect True = error_if_false(
          @"Address update output has the wrong value",
          req_value == (value.to_dict(val) |> dict.to_list(_))
        )

        check_outputs(
          rest_contract_outs,
          rest_outs
        )
      } else {
        check_outputs(
          req_outputs,
          rest_outs
        )
      }
    }

    // All required outputs have been found.
    ([], _) -> True

    _ -> error "Not all required address update outputs found"
  }
}
// Check all address updates for a writer and return the writer's address used so 
// that the writer's approval can be checked.
pub fn valid_address_update(
  inputs: List<Input>,
  outputs: List<Output>,
  redeemers: Dict<ScriptPurpose,Redeemer>,
  options_script_credential: Credential,
  proxy_hash: ScriptHash
) -> Bool {
  let req_outputs = get_input_info(
    options_script_credential,
    proxy_hash,
    inputs,
    dict.to_list(redeemers),
    []
  )

  check_outputs(req_outputs, outputs)
}
