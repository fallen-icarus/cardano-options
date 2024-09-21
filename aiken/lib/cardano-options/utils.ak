use aiken/dict.{Dict}
use aiken/option
use aiken/list
use aiken/interval.{Interval,IntervalBound,Finite}
use aiken/time.{PosixTime}
use aiken/transaction.
  { Datum
  , InlineDatum
  , Redeemer
  , ScriptPurpose
  , ValidityRange
  } as tx
use aiken/transaction/credential.
  { Address
  , Credential
  , Inline
  , StakeCredential
  , ScriptCredential
  , VerificationKeyCredential
  }
use aiken/transaction/value.{AssetName, PolicyId}

use cardano_options/data/core.{ScriptHash,KeyHash}
use cardano_options/data/datums.{OptionsDatum}

// If a test fails, provide a user friendly error message. This is useful
// when chaining if-then-elses.
pub fn trace_if_false(msg: String, predicate: Bool) {
  if !predicate {
    trace msg
    predicate
  } else {
    predicate
  }
}

// If a test fails, immediately crash the script with an error message. This is useful
// when some side-effect needs to be verified before continuing with the main function.
pub fn error_if_false(msg: String, predicate: Bool) {
  if !predicate {
    error msg
  } else {
    predicate
  }
}

// This parses the datums attached to options UTxOs. It will fail with an error unless
// the datum is an inline OptionsDatum.
pub fn parse_datum(raw_datum: Datum) -> OptionsDatum {
  when raw_datum is {
    InlineDatum(d) -> {
      expect options_datum: OptionsDatum = d
      options_datum
    }
    _ -> error @"All datums must be inline datums"
  }
}

// Check that either the staking pubkey signed or the staking script was executed.
pub fn staking_credential_approves(
  staking_cred: Option<StakeCredential>, 
  withdrawals: Dict<StakeCredential,Int>,
  extra_signatories: List<KeyHash>
) -> Bool {
  when staking_cred is {
    // This is to prevent permanent locking of funds. Beacons can never be minted to an address
    // without a valid staking credential.
    None -> True

    // If the address uses a staking pubkey, it must have signed the tx.
    Some(Inline(VerificationKeyCredential(skey))) -> list.has(extra_signatories,skey)

    // If the address uses a staking script, it must have been executed in the tx. In order for
    // the staking credential to show up in this dictionary, it must be registered.
    Some(svh) -> dict.has_key(withdrawals,svh)
  }
}

// Check if the address is using a pubkey or the proxy script for the payment credential.
// If the proxy script is used for the payment credential, the address MUST have a staking
// credential.
pub fn valid_payment_address(proxy_hash: ScriptHash, addr: Address) -> Bool {
  when addr is {
    // Any payment pubkey can be used.
    Address(VerificationKeyCredential(_),_) -> True
    // The only payment script supported is the proxy script.
    Address(ScriptCredential(sh),stake) -> sh == proxy_hash && option.is_some(stake)
  }
}

// Get the time from the tx's invalid-hereafter setting. The invalid-hereafter setting is used
// to prove some time has not passed yet. This will not crash the script if invalid-hereafter is not
// set.
pub fn get_upper_bound_optional(val_range: ValidityRange) -> Option<PosixTime> {
  when val_range is {
    Interval(_,IntervalBound(Finite(x),_)) -> Some(x)
    _ -> None
  }
}

// Get the time from the tx's invalid-before setting. The invalid-before setting is used
// to prove some time has indeed passed yet. This will not crash the script if invalid-before is not
// set.
pub fn get_lower_bound_optional(val_range: ValidityRange) -> Option<PosixTime> {
  when val_range is {
    Interval(IntervalBound(Finite(x),_),_) -> Some(x)
    _ -> None
  }
}

// Check if a credential is a pubkey credential.
pub fn is_pub_key_credential(cred: Credential) -> Bool {
  when cred is {
    VerificationKeyCredential(_) -> True
    _ -> False
  }
}

// A helper function for adding tokens to a Value.
pub fn sum_token_values(_key: AssetName, v1: Int, v2: Int) -> Option<Int> {
  Some(v1 + v2)
}

// Check if the target script was executed using either a minting execution or a staking
// execution, and with the proper redeemer.
pub fn script_executed_with_redeemer(
  redeemers: Dict<ScriptPurpose, Redeemer>,
  req_redeemer: Data, 
  req_script_id: PolicyId
) -> Bool {
  let req_mint = tx.Mint(req_script_id)
  let req_stake = tx.WithdrawFrom(Inline(ScriptCredential(req_script_id)))
  let foo = fn(red: (ScriptPurpose, Redeemer)) -> Bool {
    let (purpose,red_used) = red
    and {
      purpose == req_mint || purpose == req_stake,
      red_used == req_redeemer
    }
  }
  list.find(dict.to_list(redeemers),foo) |> option.is_some(_)
}

// Check if the target observer script was executed using a staking execution. This function is only
// safe to do in situations where the redeemer used for the staking execution does not matter.
pub fn observer_script_executed(
  withdrawals: Dict<StakeCredential, Int>,
  target_script_id: ByteArray
) -> Bool {
  dict.get(withdrawals,Inline(ScriptCredential(target_script_id))) |> option.is_some(_)
}
