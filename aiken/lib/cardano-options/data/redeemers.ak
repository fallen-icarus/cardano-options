use aiken/transaction/credential.{Address}
use aiken/transaction/value.{PolicyId}

pub type OptionsRedeemer {
  // Close or update a Proposal UTxO.
  CloseOrUpdateProposal

  // Purchase a options contract by converting a Proposal UTxO into an Active UTxO. The
  // `desiredTermsIndex` is identifies which `Terms` the buyer is purchasing.
  PurchaseContract{desired_terms_index: Int}

  // Execute an active options contract.
  ExecuteContract

  // Close an active options contract that has expired.
  CloseExpiredContract

  // Update the address where the ask asset must go. Optionally deposit additional ada if needed.
  UpdatePaymentAddress{new_address: Address, deposit_increase: Int}
}

pub type ProposalBeaconsRedeemer {
  // Create, close, or update some Proposal UTxOs (1 or more). 
  CreateCloseOrUpdateProposals

  // Burn any beacons. 
  BurnProposalBeacons

  // Register the script.
  RegisterProposalScript
}

pub type ActiveBeaconsRedeemer {
  // This redeemer allows composing purchases, executions, and closing expired in the same 
  // transaction.
  PurchaseExecuteOrCloseExpired{proposal_beacon_id: PolicyId}

  // Burn any beacons. This is used for burning leftover Key NFTs.
  BurnActiveBeacons
}

pub type AddressObserverRedeemer {
  // Observer a writer's address update transaction.
  ObserveAddressUpdate

  // Register the script.
  RegisterAddressObserverScript
}
