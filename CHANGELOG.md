# Revision history for cardano-options

## 1.0.0.0rc

#### New Features

*All* of the features discussed in the *Future Directions* section of the previous version's README
have been added:

- *Script-based payment addresses* - writers can now opt to use the pre-approved proxy script for
the address where direct payments must go. This proxy script just delegates authorization to the
address' staking credential which means writers can now use arbitrary logic to protect their payment
addresses. For example, to use a multi-sig, the payment address would use the proxy script as the
payment credential and the multi-sig script as the staking credential. This feature is likely very
important for enabling adoption by institutions and corporations. This address can be updated at any
time while the contract is active.
- *Both Contract IDs burned during execution* - there is now nothing left over after an options
contract is executed. The execution transactions cleans up all remaining UTxOs and beacons.
- *Variable Deposits* - writers can tell the protocol exactly how much was used for the contract's
minUTxOValue. This amount will be returned to the writer when the contract is executed.
- *Maximum Composability* - not only can users buy multiple options in a single transaction, but
they can also compose all protocol actions with all other protocol actions (the only exception is
burning unused key NFTs). For example, it is possible to create a new proposal contract for sale,
buy another proposal contract, execute an active contract, close an expired active contract, and
update the payment address for an active contract, **all in one transaction**. The total fee for
this composition is 0.7 ADA. Rolling over expired options contracts into new ones is just one of the
many possible compositions. This protocol can also be composed with all other DApps.
- *Assortment of possible terms per Proposal* - when a writer creates a new proposal contract for
sale, they can specify several different possible pairings of expirations, strike prices, and
premiums. For example, the writer can say "you can have expiration x with strike price y and premium
z, or you can have expiration a with strike price b and premium c." Then, when the buyer wishes to
purchase the new contract, they tell the protocol which terms they are buying. The protocol will
enforce the selected terms for the active phase. This method enabled consolidating the original
Asset UTxO and Proposal UTxOs into a single UTxO which dramatically improved performance.
- *Additional beacons* - there is now an offer asset beacon, an ask asset beacon, a premium asset
beacon, a trading pair beacon, and a contract id beacon. These new beacons increase the kinds of
queries that are possible. For example, you can look up all proposal contracts for ADA -> DJED, and
further filter them by premium asset; or you can look up all options contracts offering WMT,
regardless of the other assets involved.
- *Updating Proposals in-place* - Proposal UTxOs can now be updated in place. The first version
required two transactions to do this (closing in one transaction and creating the new UTxO in
another).

#### Optimizations

The smart contracts have been re-written in aiken, and all of the "one per tx" restrictions have
been lifted. Every part of the protocol has seen huge performance improvements over the PlutusTx
version.

The logic for the protocol was broken over several smart contracts to allow fitting more features
into the protocol. The universal options spending script just delegates to one of the other scripts
which can either be executed as minting policies or staking scripts. Since all scripts can be used
as reference scripts, there was no drawback from splitting up the logic like this.

## 1.0.0 (MVP)

* First version. Released on an unsuspecting world.
