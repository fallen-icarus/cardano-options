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
  // Create some Active UTxOs (1 or more) by buying Proposal UTxOs. The CurrencySymbol is the 
  // policy id for the proposal beacons.
  PurchaseContracts{proposal_beacon_id: PolicyId}

  // Burn the lock and key NFT to execute the options contracts.
  ExecuteContracts

  // Burn all beacons to claim expired options contracts.
  CloseExpiredContracts

  // Burn any beacons.
  BurnActiveBeacons
}

pub type AddressObserverRedeemer {
  // Observer a writer's address update transaction.
  ObserveAddressUpdate

  // Register the script.
  RegisterAddressObserverScript
}
