use aiken/dict
use aiken/transaction.{Transaction,Input,Output,ScriptContext} as tx
use aiken/transaction/certificate.{CredentialRegistration}
use aiken/transaction/credential.
  { Address
  , Inline
  , ScriptCredential
  }
use aiken/transaction/value.{PolicyId}

use cardano_options/proposal_validation as propose
use cardano_options/purchase_validation as purchase
use cardano_options/execute_validation as execution
use cardano_options/close_expired_validation as close_expired
use cardano_options/address_update_validation as update
use cardano_options/utils.{trace_if_false}
use cardano_options/data/core.{ScriptHash}
use cardano_options/data/datums.{OptionsDatum,ProposalDatum,ActiveDatum}
use cardano_options/data/redeemers.
  { OptionsRedeemer
  , ProposalBeaconsRedeemer
  , ActiveBeaconsRedeemer
  , AddressObserverRedeemer
  }

// ----------------------------------------------
// Options Validator Script
// ----------------------------------------------
// This smart contract is the payment credential for all options addresses. All options contracts
// for sale and all active options contracts are kept in addresses using this smart contract as the
// payment credential. All option writers get their own options address by using a staking
// credential unique to them; this smart contract delegates all owner related actions to the staking
// credential used for the address. In order to minimize redundant executions, this smart contract
// also delegates protocol logic to observer scripts when needed.
validator() {
  fn options_script(datum: OptionsDatum, redeemer: OptionsRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      redeemers.CloseOrUpdateProposal -> {
        // This redeemer is dedicated to closing or updating any Proposal UTxO. Invalid Proposal
        // UTxOs (ie, UTxOs with a ProposalDatum but no beacons) can also be closed with this
        // redeemer.
        //
        // This address' staking credential must signal approval, if the UTxO contains beacons, the
        // proposal_beacon_script must be executed with the CreateCloseOrUpdateProposals redeemer.

        // Extract the required information from the script context.
        expect ScriptContext(
          Transaction{inputs,redeemers,withdrawals,extra_signatories..}, 
          tx.Spend(out_ref)
        ) = ctx

        // Get this input being spent to extract the borrower's staking credential.
        expect Some(Input(_, Output(Address(_,stake), val, ..))) = tx.find_input(inputs,out_ref)


        // Get the proposal beacon id from the datum.
        let proposal_beacon_policy_id = when datum is {
          ProposalDatum{proposal_beacon_policy_id,..} -> proposal_beacon_policy_id
          _ -> error @"UTxO is not a Proposal UTxO"
        }

        let req_redeemer: Data = redeemers.CreateCloseOrUpdateProposals // Cast to Data type.

        and {
          trace_if_false( 
            @"Writer did not approve",
            utils.staking_credential_approves(stake,withdrawals,extra_signatories)
          ),

          when value.tokens(val,proposal_beacon_policy_id) |> dict.to_list(_) is {
            [] -> True
            _ -> trace_if_false(
              @"Proposal beacon script not executed with proper redeemer",
              utils.script_executed_with_redeemer(
                redeemers,
                req_redeemer,
                proposal_beacon_policy_id
              )
            )
          }
        }
      }

      redeemers.PurchaseContract{..} -> {

        // This redeemer is dedicated to purchasing options contracts from this address. The
        // spending script just needs to ensure the active beacon script is executed properly.
        //
        // In order for this redeemer to successfully execute, all of the following must be true:
        // 1) The input must have either a ProposalDatum.
        // 2) The active beacon script must be executed using the 
        //    PurchaseContracts(proposal_beacon_id) redeemer where the proposal_beacon_id is the 
        //    proposal_beacon_id from the input's datum.

        // Get the required information from the script context.
        expect ScriptContext(Transaction{redeemers,..}, tx.Spend(_)) = ctx

        // Get the beacon ids from the ProposalDatum.
        let (proposal_beacon_policy_id,active_beacon_policy_id) = when datum is {
          ProposalDatum{proposal_beacon_policy_id,active_beacon_policy_id,..} -> 
            (proposal_beacon_policy_id,active_beacon_policy_id)
          _ -> error @"UTxO is not a Proposal UTxO"
        }

        // The active beacon script must be executed using the proper redeemer. The redeemer map
        // must be used to check for execution because the proposal_beacon_policy_id must be
        // correctly enforced.
        let req_redeemer: Data = // Cast to Data type.
          redeemers.PurchaseContracts(proposal_beacon_policy_id)
        trace_if_false(
          @"Active beacon script not executed with proper redeemer",
          // It must be executed as a minted policy.
          dict.get(redeemers, tx.Mint(active_beacon_policy_id)) == Some(req_redeemer)
        )
      }

      redeemers.ExecuteContract -> {
        // Executing contracts involves burning all beacons attached to the Active UTxOs as well as
        // the Key NFT for each contract. Because of this burning requirement, the logic is
        // outsourced to the active beacon script. This redeemer just needs to ensure the beacon
        // script is executed with the proper redeemer.
        //
        // By requiring both the Lock NFT and the Key NFT to be burned, only the owner of the
        // Key NFT is able to spend UTxOs with this redeemer.
        //
        // The contract must be not expired. This will be checked by the active beacon script.

        // Extract out the required information from the script context.
        expect ScriptContext(Transaction{redeemers,..}, tx.Spend(_)) = ctx

        // Get the active_beacon_id from the datum.
        let active_beacon_policy_id = when datum is {
          ActiveDatum{active_beacon_policy_id,..} -> active_beacon_policy_id
          _ -> error @"UTxO is not an Active UTxO"
        }

        // The active beacon script will check that the required Key is burned in the transaction
        // and that this loan is actually expired.
        let req_redeemer: Data = redeemers.ExecuteContracts
        trace_if_false(
          @"Active beacon script not executed with proper redeemer",
          dict.get(redeemers, tx.Mint(active_beacon_policy_id)) == Some(req_redeemer)
        )
      }

      redeemers.CloseExpiredContract -> {
        // This redeemer can be used to close expired contracts and spend invalid Active UTxOs (ie,
        // UTxOs with an ActiveDatum but no beacons).
        //
        // If the UTxO contains beacons, then the active_beacon_script must be executed with the
        // CloseExpiredContracts redeemer. Otherwise, this script will check for the writer's
        // approval.

        // Extract out the required information from the script context.
        expect ScriptContext(
          Transaction{inputs,withdrawals,extra_signatories,redeemers,..},
          tx.Spend(out_ref)
        ) = ctx

        // Get the address and value of the input being spent.
        expect Some(Input(_,Output(Address(_,stake),utxo_value,..))) = tx.find_input(inputs,out_ref)

        // Get the active_beacon_id from the datum.
        let active_beacon_policy_id = when datum is {
          ActiveDatum{active_beacon_policy_id,..} -> active_beacon_policy_id
          _ -> error @"UTxO is not an Active UTxO"
        }

        // If there are beacons in this UTxO, the active beacon script must be executed with
        // the proper redeemer.
        when value.tokens(utxo_value,active_beacon_policy_id) |> dict.to_list(_) is {
          [] -> trace_if_false(
            @"Writer credential did not approve", 
            utils.staking_credential_approves(stake,withdrawals,extra_signatories)
          )

          _ -> {
            // The active beacon script must be exectued with the proper redeemer.
            let req_redeemer: Data = redeemers.CloseExpiredContracts
            trace_if_false(
              @"Active beacon script not executed with proper redeemer",
              dict.get(redeemers, tx.Mint(active_beacon_policy_id)) == Some(req_redeemer)
            )
          }
        }
      }

      redeemers.UpdatePaymentAddress{..} -> {
        // This redeemer delegates all checks to the address_update_observer_script. All this redeemer
        // must do is ensure the address_update_observer_script script is executed as a staking
        // script.

        // Extract out the required information from the script context.
        expect ScriptContext(Transaction{withdrawals,..}, tx.Spend(_)) = ctx

        // Get the address_update_observer_hash from the Active datum. All valid Active UTxOs are 
        // guaranteed to have the proper address_update_observer_hash in the datum.
        let address_update_observer_hash = when datum is {
          ActiveDatum{address_observer_hash,..} -> address_observer_hash
          _ -> error @"UTxO is not an Active UTxO"
        }

        // The address_update_observer_script must be executed with the ObserveAddressUpdate redeemer. This
        // redeemer can only be used with a staking execution. The address_update_observer_script only has
        // one possible redeemer that can be used.
        trace_if_false(
          @"Address update observer script not executed with proper redeemer",
          utils.observer_script_executed(
            withdrawals,
            address_update_observer_hash
          )
        )
      }
    }
  }
}

// ----------------------------------------------
// Address Update Observer Script
// ----------------------------------------------
// To minimize the redundant executions from having the options validator script do the checks, this
// observer script is used to validate lender address updates of active contracts. In order to
// observe updates, this script must be executed as a staking script. The options validator script's
// hash is hard-coded into this observer script so that the observer script allways knows where to
// look. The beacons can only be minted if the address update observer script hash in the
// ActiveDatum is correct. The proxy_hash is also hard-coded into this observer scripts so that new
// addresses can be checked for validity.
validator(proxy_hash: ScriptHash, options_script_hash: ScriptHash) {
  fn address_observer_script(redeemer: AddressObserverRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      redeemers.RegisterAddressObserverScript -> {
        // Only allow registering the script. It cannot be delegated or deregistered.
        when ctx is {
          ScriptContext(_, tx.Publish(CredentialRegistration(_))) -> True
          _ -> error "This redeemer can only be used to register the address update observer script"
        }
      }

      redeemers.ObserveAddressUpdate -> {
        // Just in case the writer needs to update the payment address of an Active contract,
        // this redeemer can oversee all address updates for that writer.
        //
        // This redeemer can only be used with a staking execution. It will compare the redeemer
        // map to the inputs to only validate address updates. Addresses for expired contracts
        // cannot be updated.

        // In order for this redeemer to successfully execute, all of the following must be true:
        // 1) The address_observer_script is executed as a staking script.
        // 2) All contract inputs must be for the same writer.
        // 3) The writer must signal approval.
        // 4) For all contract inputs:
        //      a) It must have an ActiveDatum.
        //      b) The loan must not be expired.
        //      c) The required beacons must be present.
        //      d) The deposit_increase must be >= 0.
        //      e) The new address must either use a payment pubkey or the proxy script with a
        //      valid staking credential.
        //      f) There must be a corresponding Active UTxO output to the writer's address with:
        //         1) The value must be starting value + ada deposit_increase

        // Extract the required information from the script context.
        expect ScriptContext( 
          Transaction{inputs,outputs,validity_range,redeemers,withdrawals,extra_signatories,..}, 
          tx.WithdrawFrom(_)
        ) = ctx

        // This has its own error messages.
        let Address(_,writer_stake_cred) = update.valid_address_update(
          inputs,
          outputs,
          redeemers,
          options_script_hash,
          proxy_hash,
          utils.get_upper_bound(validity_range),
        )

        // The writer must approve.
        trace_if_false(
          @"Writer credential did not approve", 
          utils.staking_credential_approves(writer_stake_cred,withdrawals,extra_signatories)
        )
      }
    }
  }
}

// ----------------------------------------------
// Active Beacon Script
// ----------------------------------------------
// This script governs the minting and burning of all beacons for active contracts. Active contracts
// are options contracts that are for sale. It can only be executed as a minting script. The
// observer hash is hard-coded into the script so that it can enforce the use of the proper observer
// logic. The options_script_hash is hard-coded into the script so that it knows where to look.
// There are four proposal beacons; every Active UTxO must have all four, and no extraneous
// beacons. The four beacons are:
// 1) TradingPair beacon: sha2_256(offer_id ++ offer_name ++ ask_id ++ ask_name) except when one of
//    the assets is ADA, that policy id is replaced with "00".
// 2) Offer beacon: sha2_256("01" ++ offer_id ++ offer_name)
// 3) Ask beacon: sha2_256("02" ++ ask_id ++ ask_name)
// 4) ContractId beacon: sha2_256(proposal_utxo_tx_hash ++ proposal_utxo_output_index)
//
// There are two copies of the ContractId beacon for every contract. These are the Lock & Key NFTs
// for that contract. One gets stored with the contract, the other is claimed by the buyer.
validator(options_script_hash: ScriptHash, address_observer_script_hash: ScriptHash) {
  fn active_beacon_script(redeemer: ActiveBeaconsRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      redeemers.PurchaseContracts{proposal_beacon_id} -> {
        // This redeemer will only check inputs spent with the PurchaseContract spending redeemer.
        //
        // For this redeemer to successfully execute, all of the following must be true:
        // 1) Every proposal input spent with PurchaseContract must have a ProposalDatum with the
        //    required beacons.
        // 2) Every proposal must have a corresponding Active contract output:
        //    a) Locked at the proper writer's options address.
        //    b) The datum must be an inline ActiveDatum and have the proper terms:
        //       - active_beacon_policy_id == this policy id
        //       - address_observer_hash == hard-coded address_observer_script_hash
        //       - offer_asset == offer_asset from proposal
        //       - offer_quantity == offer_quantity from proposal
        //       - ask_asset == ask_asset from proposal
        //       - trading_pair_beacon == trading_pair_beacon from proposal
        //       - offer_asset_beacon == offer_asset_beacon from proposal
        //       - ask_asset_beacon == ask_asset_beacon from proposal
        //       - strike_price == strike_price from the Terms at the index specified by the
        //         spending redeemer for this proposal
        //       - expiration == expiration from the Terms at the index specified by the spending
        //         redeemer for this proposal
        //       - contract_deposit == contract_deposit from proposal
        //       - payment_address == payment_address from proposal
        //       - contract_id == ContractId beacon name for the proposal UTxO
        //    c) It must have exactly four active beacons (one unit of each):
        //       - The TradingPair beacon for the associated proposal
        //       - The Offer beacon for the associated proposal
        //       - The Ask beacon for the associated proposal
        //       - The ContractId beacon for the associated proposal
        //    d) It must have the offer_quantity + the contract_deposit.
        // 2) Every proposal must have a corresponding premium output:
        //    a) Locked at the payment_address from the proposal.
        //    b) Contains the premium from the Terms at the index specified by the spending redeemer
        //       for this proposal.
        //    c) Contains the proper hard-coded datum: (active_beacon_policy_id,contract_id)
        // 3) Either all proposal beacons attached to the proposal inputs must be burned or the
        //    proposal_beacon_script must be executed using CreateCloseOrUpdateProposals.
        // 4) The active_beacon_script must mint exactly:
        //    a) 1 TradingPair beacon for each proposal that requires it
        //    b) 1 Offer beacon for each proposal that requires it
        //    b) 1 Ask beacon for each proposal that requires it
        //    b) 2 ContractId beacon for each proposal that requires it
        //
        // There is no need to check whether the purchased contracts are expired since buyers are
        // incentivized NOT to buy expired contracts.
        //
        // Every proposal has two required outputs: a premium payment output, and a contract output.
        // The premium payment must be the amount that is associated with the selected strike_price
        // and expiration.
        //
        // The index passed in with the spending redeemer PurchaseContract tells the
        // active_beacon_script which contract terms the buyer is looking to purchase.

        // Extract out the requried information from the script context. This redeemer can only
        // be used with a minting execution.
        expect ScriptContext( 
          Transaction{inputs,outputs,mint,redeemers,..}, 
          tx.Mint(active_beacon_id) 
        ) = ctx

        // This has its own error messages.
        purchase.valid_purchase(
          inputs,
          outputs,
          redeemers,
          value.from_minted_value(mint),
          options_script_hash,
          address_observer_script_hash,
          core.to_proposal_beacon_id(proposal_beacon_id),
          core.to_active_beacon_id(active_beacon_id)
        )
      }

      redeemers.ExecuteContracts -> {
        // This redeemer will only process inputs being spent with the ExecuteContract spending
        // redeemer. 
        //
        // The invalid-hereafter flag is used to prove that the expiration time has not actually
        // passed. When executing multiple contracts, this flag should be set to the earliest
        // expiration time.
        //
        // To successfully execute a contract, all of the following must be true:
        // 1) All contract inputs must be Active contracts that have not expired.
        // 2) All contract inputs must contain the required beacons.
        // 3) Both the lock & key NFT for each contract must be burned.
        // 4) All active beacons among the inputs must be burned.
        //
        // Since the Key NFT is actually being burned in this transaction (ie, the UTxO is being
        // consumed), this can be used as a proxy for the contract owner's approval.
        //
        // A user is able to execute multiple contracts in a given transaction as long as they 
        // control all the required Key NFTs.

        // Extract out the required information from the script context. This redeemer can only be
        // used with a minting execution.
        expect ScriptContext( 
          Transaction{inputs,outputs,mint,validity_range,redeemers,..}, 
          tx.Mint(active_beacon_id) 
        ) = ctx

        // This has its own error messages.
        execution.valid_execution(
          options_script_hash,
          utils.get_upper_bound(validity_range),
          inputs,
          outputs,
          redeemers,
          value.from_minted_value(mint) |> value.tokens(_,active_beacon_id)
        )
      }

      redeemers.CloseExpiredContracts -> {
        // This redeemer will only process inputs being spent with the CloseExpiredContract spending
        // redeemer. 
        //
        // The invalid-before flag is used to prove that the expiration time has actually passed.
        // When closing multiple expired contracts, this flag should be set to the current time.
        //
        // To successfully close an expired contract, all of the following must be true:
        // 1) All contract inputs must be Active contracts that have expired.
        // 2) All contract inputs must contain the required beacons.
        // 3) All active beacons among the inputs must be burned.
        // 4) All contract inputs must belong to the same writer.
        // 5) The writer must approve the transaction.

        // Extract out the required information from the script context. This redeemer can only be
        // used with a minting execution.
        expect ScriptContext( 
          Transaction{inputs,mint,validity_range,withdrawals,extra_signatories,redeemers,..}, 
          tx.Mint(active_beacon_id) 
        ) = ctx

        // This has its own error messages.
        let Address(_,writer_stake_cred) = close_expired.valid_close(
          core.to_active_beacon_id(active_beacon_id),
          options_script_hash,
          utils.get_lower_bound(validity_range),
          inputs,
          redeemers,
          value.from_minted_value(mint) |> value.tokens(_,active_beacon_id)
        )

        // The writer must approve.
        trace_if_false(
          @"Writer credential did not approve", 
          utils.staking_credential_approves(writer_stake_cred,withdrawals,extra_signatories)
        )
      }

      redeemers.BurnActiveBeacons -> {
        // This redeemer will always succeed as long as it is only used to burn active beacons.
        // This is useful the Key NFT is not used to execute a contract and now needs to be cleaned
        // up.

        // Get the required information from the script context. This redeemer can only be used
        // with minting executions.
        expect ScriptContext(Transaction{mint,..}, tx.Mint(beacon_id)) = ctx

        // Check that proposal beacons are only burned.
        trace_if_false( 
          @"This redeemer can only be used to burn active beacons",
          dict.foldl( 
            value.from_minted_value(mint) |> value.tokens(_,beacon_id),
            True, 
            fn(_,v,r) {r && v < 0} // Burn check.
          )
        )
      }
    }
  }
}

// ----------------------------------------------
// Proposal Beacon Script
// ----------------------------------------------
// This script governs the minting and burning of all beacons for proposals. Proposals are options
// contracts that are for sale. It can be executed as either a staking script or a minting script,
// depending on the redeemer. The active_beacon_id is hard-coded into the script so that the
// proposal script determines which scripts can be used for the Active phase. Since the observer
// script hashes are included in the serialized active beacon script, if a malicious user tries to
// use different observer scripts, the proposal beacons will be different and will therefore, not
// even be discoverable by normal users.
//
// There are four proposal beacons; every Proposal UTxO must have all four, and no extraneous
// beacons. The four beacons are:
// 1) TradingPair beacon: sha2_256(offer_id ++ offer_name ++ ask_id ++ ask_name) except when one of
//    the assets is ADA, that policy id is replaced with "00".
// 2) Offer beacon: sha2_256("01" ++ offer_id ++ offer_name)
// 3) Ask beacon: sha2_256("02" ++ ask_id ++ ask_name)
// 4) PremiumAsset beacon: sha2_256("03" ++ premium_id ++ premium_name) 
validator(proxy_hash: ScriptHash, options_script_hash: ScriptHash, active_beacon_id: PolicyId) {
  fn proposal_beacon_script(redeemer: ProposalBeaconsRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      redeemers.CreateCloseOrUpdateProposals -> {
        // This redeemer is used to create proposals for sale. If proposals already exist, this
        // redeemer can be used to either close or update its terms. All proposal outputs in this
        // transaction must go to an options address with a valid staking credential.
        //
        // By explicitly stating what valid UTxOs with proposal beacons are, it implicitly enforces
        // that the proper beacons are minted/burned. This design allows for closing and updating
        // Proposal UTxOs in the same transaction while also improving performance. This means
        // converting Proposals UTxOs in place is supported.
        //
        // To enable updating Proposal UTxOs even when no beacons need to be minted/burned, this
        // redeemer can be used with a staking execution.
        //
        // For this redeemer to successfully execute, all of the following must be true:
        // 1) All proposal beacons must be locked at a options address with a valid staking
        //    credential.
        // 2) All outputs with proposal beacons must have exactly four beacons (one unit of each):
        //    a) The TradingPair beacon
        //    b) The Offer beacon
        //    c) The Ask beacon
        //    d) The PremiumAsset beacon
        // 3) All outputs with proposal beacons must have the proper inline ProposalDatum:
        //    a) proposal_beacon_policy_id == this policy id
        //    b) active_beacon_policy_id == hard-coded active_beacon_id
        //    c) offer_asset must not be a proposal beacon
        //    d) offer_quantity > 0
        //    e) ask_asset must not be a proposal beacon
        //    f) trading_pair_beacon must correspond to the TradingPair beacon name for this
        //       contract
        //    g) offer_asset_beacon must correspond to the Offer beacon name for this contract
        //    h) ask_asset_beacon must correspond to the Ask beacon name for this contract
        //    i) premium_asset must not be a protocol beacon
        //    j) premium_asset_beacon must correspond to the premium_asset for this contract
        //    k) contract_deposit > 0
        //    l) payment_address must either use a payment pubkey or the proxy script with a valid
        //       staking credential.
        //    m) For all possible_terms:
        //       1. The strike_price numerator must be > 0 and the denominator must be > 0
        //       2. The expiration must be >= invalid-hereafter
        //       3. The premium must be > 0
        //    n) writer_credential must match the stake credential used in in the destination
        //       options address.
        //    o) offer_asset /= ask_asset
        // 4) All outputs with proposal beacons must contain exactly the contract_deposit and the
        //    offer_quantity amount of the offer_asset. No other assets are allowed in the output.
        //
        // In order to help prevent creating a bunch of expired contracts to clutter the beacon
        // queries, all contract expirations must be set to a time that has not passed yet. The
        // invalid-hereafter bound is used to prove that time x has not passed yet, and all
        // expirations must be >= x. This is how the smart contract can prove that all of the newly
        // created options contracts are not already expired.
        //
        // In TradFi, it is possible for writers to create many different contracts backed by the
        // same assets. Buyers pick which contract they are interested in and the assets get paired
        // with the chosen contract. To enable this feature, writers can place their desired terms
        // in the possible_terms list. Only the expiration, strike_price, and premium are allowed to
        // vary across the assortment.

        // Extract out the required information from the script context.
        let ScriptContext(Transaction{outputs,validity_range..}, purpose) = ctx

        // This redeemer must be used with either a staking execution or a minting execution. Get
        // the proposal_beacon_id from the purpose while checking the type of execution used.
        let proposal_beacon_id = when purpose is {
          tx.Mint(proposal_beacon_id) -> proposal_beacon_id
          tx.WithdrawFrom(Inline(ScriptCredential(proposal_beacon_id))) -> proposal_beacon_id
          _ -> error @"This redeemer must be used with minting or staking execution"
        }

        // Check outputs. This has its own trace messages.
        propose.proposal_beacons_destination_check(
          options_script_hash,
          core.to_proposal_beacon_id(proposal_beacon_id),
          core.to_active_beacon_id(active_beacon_id),
          proxy_hash,
          outputs,
          utils.get_upper_bound(validity_range)
        )
      }

      redeemers.BurnProposalBeacons -> {
        // This redeemer will always succeed as long as it is only used to burn proposal beacons.
        // This is useful whenever another script enforces which beacons are burned. An example is
        // when an options contract is executed.

        // Get the required information from the script context. This redeemer can only be used
        // with minting executions.
        expect ScriptContext(Transaction{mint,..}, tx.Mint(beacon_id)) = ctx

        // Check that proposal beacons are only burned.
        trace_if_false( 
          @"This redeemer can only be used to burn proposal beacons",
          dict.foldl( 
            value.from_minted_value(mint) |> value.tokens(_,beacon_id),
            True, 
            fn(_,v,r) {r && v < 0} // Burn check.
          )
        )
      }

      redeemers.RegisterProposalScript -> {
        // Only allow registering the script. It cannot be delegated or deregistered.
        when ctx is {
          ScriptContext(_, tx.Publish(CredentialRegistration(_))) -> True
          _ -> error "This redeemer can only be used to register the proposal beacon script"
        }
      }
    }
  }
}

