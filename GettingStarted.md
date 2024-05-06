# Getting Started

The `cardano-options` CLI assumes that all transactions are built and signed using `cardano-cli`.
**Access to a local node is not necessary**, although it does simplify things. Koios can be used for
all steps that require access to a node.

Template bash scripts that follow these steps are available [here](scripts/). There are only
examples using a local node. Using a remote node requires extra steps since the transaction must be
manually balanced. If you would like to use a remote node, the `cardano-options` CLI supports
everything you need. You can cross-reference these local node template scripts with the
[cardano-swaps](https://github.com/fallen-icarus/cardano-swaps/tree/main/scripts) remote node
template scripts to come up with your own remote node template scripts for cardano-options.

## Table of Contents
- [Installing](#installing)
- [Aiken For Developers](#aiken-for-developers)
- [Overspent Budget](#overspent-budget)
- [Using Remote Nodes](#using-remote-nodes)
- [Minting Test Tokens](#minting-test-tokens)
- [Registering Scripts - DEVELOPERS ONLY](#registering-the-scripts-for-staking-execution---developers-only)
- [Creating Reference Scripts](#creating-reference-scripts)
- [Creating a Proposal UTxO](#creating-a-proposal-utxo)
- [Closing a Proposal UTxO](#closing-a-proposal-utxo)
- [Updating a Proposal UTxO](#updating-a-proposal-utxo)
- [Purchasing a Proposal UTxO](#purchasing-a-proposal-utxo)
- [Updating a Payment Address](#updating-a-payment-address)
- [Executing an Active UTxO](#executing-an-active-utxo)
- [Closing an Expired Active UTxO](#closing-an-expired-active-utxo)
- [Queries](#queries)
  - [Querying Personal Addresses](#querying-personal-addresses)
  - [Querying Proposal UTxOs](#querying-proposal-utxos)
  - [Querying Active UTxOs](#querying-active-utxos)
  - [Querying the Current Time](#querying-the-current-time)

## Installing

Make sure `cardano-cli` is also installed. You can get the most up-to-date copy from IOG's
cardano-node repo [here](https://github.com/IntersectMBO/cardano-node/releases). It will be in the
cardano-node tarball under the latest release.

### Install the necessary packages - similar to cardano-node
```bash
sudo apt update
sudo apt upgrade
sudo apt-get install autoconf automake build-essential curl g++ git jq libffi-dev libgmp-dev libncursesw5 libssl-dev libsystemd-dev libtinfo-dev libtool make pkg-config wget zlib1g-dev liblzma-dev libpq-dev
```

### Install GHC and Cabal
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

- Prepend or append the required PATH variable.
- You do not need to install the haskell-langauge-server.
- You do not need to install stack.
- Install the required packages. You can keep this terminal window open and install from another
window.
- Press ENTER to proceed.

```bash
source $HOME/.bashrc
ghcup install ghc 9.6.5
```

### Install libsodium, scep256k1, and blst
```bash
git clone https://github.com/intersectmbo/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install

cd ../ # Leave the libsodium directory.
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout ac83be33
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
make check
sudo make install
sudo ldconfig

cd ../ # Leave the secp256k1 directory.
git clone https://github.com/supranational/blst
cd blst
git checkout v0.3.10
./build.sh
cat > libblst.pc << EOF # This command extends until the next EOF
prefix=/usr/local
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: Multilingual BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: 0.3.10
Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
EOF
sudo cp libblst.pc /usr/local/lib/pkgconfig/
sudo cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/local/include/
sudo cp libblst.a /usr/local/lib
sudo chmod u=rw,go=r /usr/local/{lib/{libblst.a,pkgconfig/libblst.pc},include/{blst.{h,hpp},blst_aux.h}}
```

You need to execute the following to make the new packages usable:
```bash
echo '' >> $HOME/.bashrc # Add a newline to your .bashrc file.
echo 'export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"' >> $HOME/.bashrc
echo 'export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"' >> $HOME/.bashrc
source $HOME/.bashrc
```

### Build the executable - this may take about 30 minutes
```bash
cd ../ # Leave the blst directory.
git clone https://github.com/fallen-icarus/cardano-options
cd cardano-options
cabal clean
cabal update
cabal build exe:cardano-options
```

The `cardano-options` CLI program should now be at
`dist-newstyle/build/x86_64-linux/ghc-9.6.5/cardano-options-1.0.0.0/x/cardano-options/build/cardano-options/cardano-options`.
Move the program to somewhere in your `$PATH`.

All `cardano-options` subcommands have an associated `--help` option. The functionality is meant to
feel like `cardano-cli`.

The smart contracts are compiled *into* the created `cardano-options` CLI. The executable has
everything you need for using the protocol. It is a batteries included CLI.

## Aiken For Developers

The aiken smart contracts come precompiled but if you would like to make changes or wish to confirm
the compiled scripts yourself, you will also need to install `aiken`. You can install `aiken` using
cargo like this:

```bash
cargo install aiken --version 1.0.26-alpha
```

Make sure you install verison 1.0.26-alpha. Newer versions may change some things and so the source
code may not compile or may result in a different script. As aiken stabilizes, the code will be
updated to the latest version.

When building the protocol's blueprints, make sure to use

```bash
aiken build -f user-defined -t verbose
```

or else the user friendly error messages will be stripped from the smart contracts and the resulting
beacons will be different.

If you would like to use the `cardano-options` CLI after making your changes, you will need to
rebuild it with `cabal bulid exe:cardano-options`. As long as you did not make any breaking changes,
the CLI should still work for you.

If you would like to test your changes, you can run the tests using `cabal run tests`. As long
as you did not make any breaking changes, the tests should quickly give you feedback. There are
four kinds of tests:

1) Regression tests - tests for features that should work.
2) Failure tests - tests for scenarios that are supposed to fail.
3) Bench tests - tests to check for degraded performance in specific scenarios.
4) Performance Increase tests - tests to check for improved performance in specific scenarios; these
tests will fail if performance increases to alert you of the change.

To see the documentation for the tests, you can build the haddocks for the tests using `cabal
haddock tests`. The documentation may be easier to read than the source code. You can view the
documentation in any browser.

## Overspent Budget

While `cardano-cli` is able to auto-balance transactions, the auto-balancer does not always work
when scripts are executed in a transaction where native tokens must go to the change address. It
does not properly add the change *before* estimating the execution budgets for the transaction which
always results in it under-estimating the required execution units needed by the scripts. There are
open issues about this [here](https://github.com/input-output-hk/cardano-node/issues/5386) and
[here](https://github.com/input-output-hk/cardano-api/issues/302). If you ever see a very long and
confusing error message while using `cardano-cli transaction build`, this is probably the issue.

As a work around, whenever you build a transaction using `cardano-cli transaction build` where
scripts are being executed, you must manually create an output that has all of the native tokens
that would normally go into the change output. You can let the auto-balancer balance the ada.

## Using Remote Nodes

`cardano-cli transaction build` requires a local node for the auto-balancer which means it cannot be
used to build a transaction for a remote node. Instead, the `cardano-cli transaction build-raw` 
command is required. This command requires the following steps:
1. Build a temporary transaction that is missing the execution units and transaciton fee but is
   properly balanced. You can assume a fee of zero for this transaction.
2. Submit the temporary transaction for execution budget estimations.
3. Rebuild the transaction with the proper execution budgets. The fee is still set to zero.
4. Calculate the required fee for this new temporary transaction.
5. Create the final transaction with the required fee and properly balanced outputs (subtract off
   the fee from the change).
6. Sign the transaction and submit to a remote node.

The `cardano-options` CLI uses [Koios](https://koios.rest/) in all scenarios where a node is required.

##### Exporting protocol parameters

Some of the above steps will require the current protocol parameters. The `cardano-options` CLI had
the preproduction testnet and mainnet protocol parameters compiled into the executable when it was
built with `cabal build exe:cardano-options`. The parameters are already formatted in the way
`cardano-cli` requires. To export the parameters, you can use:
```bash
cardano-options protocol-params \
  --testnet \
  --out-file protocolParams.json
```

##### Estimating execution budgets

Submitting a transaction for execution budget estimations can be done with this command:
```bash
cardano-options evaluate-tx \
  --testnet \
  --tx-file tx.body
```

This action uses Koios. The returned budgets will be indexed by the input order, policy id order,
and withdrawal credential order. **This may not be the same order you specified when building the
temporary transaction.** The node will reorder them based on lexicographical ordering. If you are
not sure of the proper ordering, you can view the transaction file that is created with
`cardano-cli` using `cardano-cli transaction view`; the inputs, policy ids, and withdrawal
credentials will be properly ordered.

##### Submitting the final transaction

Submitting the final transaction for addition to the blockchain can be done with this command:
```bash
cardano-options submit \
  --testnet \
  --tx-file tx.signed
```

The transaction will be submitted through Koios.

## Minting Test Tokens

An always succeeding minting policy as well as the required redeemer are included with template bash
scripts for a local node. These can be used to create as many native tokens as needed to test this
protocol.

To see how to mint test tokens using a local node, refer
[here](scripts/local-node/mint-test-tokens/)

## Registering the scripts for staking execution - DEVELOPERS ONLY

**This action only needs to be done once per network for the entire protocol. It does not need to be
done by any users.** These instructions are for completeness as they may be needed by developers.

The plutus scripts cannot be executed as staking scripts until after they are registered. Once the
scripts are registered, they can be used as staking scripts immediately by all users. Registering
the scripts does not require executing the scripts (this may change in the future). Once they are
registered, *they cannot be deregistered*.

**Registration has already been done for all scripts for the preproduction testnet.**

The following protocol scripts requires staking executions, and therefore, require registration:
- Proposal Beacon Script
- Address Update Observer Script

Registering the scripts involve:
1. Exporting each script from the `cardano-options` CLI.
2. Creating a registration certificate for each script.
3. Submitting a transaction that also pays the registration deposit (2 ADA per registration).

All scripts can be registered in a single transaction (this may change if registration eventually
requires executing each script).

##### Exporting the scripts
```bash
cardano-options scripts \
  --proposal-script \
  --out-file proposal_beacons.plutus

cardano-options scripts \
  --address-update-script \
  --out-file address_update_observer.plutus
```

##### Create the registration certificates
```bash
cardano-cli stake-address registration-certificate \
  --stake-script-file proposal_beacons.plutus \
  --out-file proposal_beacons.cert

cardano-cli stake-address registration-certificate \
  --stake-script-file address_update_observer.plutus \
  --out-file address_update_observer.cert
```

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local-node/create-reference-scripts/register-scripts.sh)

## Creating Reference Scripts

**Do not skip this step!** While beacon tokens can be used to trustlessly share reference scripts,
this has not been set up for the beta testing. For now, you will need your own reference scripts.

Creating reference scripts involves the following steps:
1. Export the scripts from the `cardano-options` CLI.
2. Submit a transaction with the reference script stored in the outputs.

Due to the sizes of the scripts, each script will require its own transaction.

You can see examples [here](scripts/local-node/create-reference-scripts/).

## Creating a Proposal UTxO

Creating a Proposal UTxO involves the following steps:
1. Create your writer address.
2. Calculate the required beacon names to mint.
3. Create the required beacon script redeemer.
4. Create the required ProposalDatum for each Proposal UTxO you wish to create.
5. Submit a transaction that creates the Proposal UTxOs.

##### Creating your writer address
```bash
# Export the spending script.
cardano-options scripts options-script \
  --out-file options.plutus

# Create the writer address.
cardano-cli address build \
  --payment-script-file options.plutus \
  --stake-verification-key-file writer_stake.vkey \
  --testnet-magic 1 \
  --out-file writer.addr
```

Cardano-Options also supports using staking scripts for the address. To use a staking script, use
the `--stake-script-file` flag instead of the `--stake-verification-key-file` flag.

For a mainnet address, just use the `--mainnet` flag instead of `--testnet-magic 1` when creating
the address.

##### Calculate the required beacon names to mint
Proposal UTxOs require four proposal beacons: 
- Offer beacon
- Ask beacon
- Premium beacon
- Trading pair beacon

```bash
## Get the proposal beacon policy id.
beaconPolicyId=$(cardano-options beacon-name policy-id \
  --proposal-beacons \
  --stdout) 

## Get the required beacon names.
offerBeaconName=$(cardano-options beacon-name asset-name offer-beacon \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --stdout)
askBeaconName=$(cardano-options beacon-name asset-name ask-beacon \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --stdout)
premiumBeaconName=$(cardano-options beacon-name asset-name premium-beacon \
  --premium-asset 'lovelace' \
  --stdout)
pairBeaconName=$(cardano-options beacon-name asset-name trading-pair-beacon \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --stdout)

offerBeacon="${beaconPolicyId}.${offerBeaconName}"
askBeacon="${beaconPolicyId}.${askBeaconName}"
premiumBeacon="${beaconPolicyId}.${premiumBeaconName}"
pairBeacon="${beaconPolicyId}.${pairBeaconName}"
```

##### Create the required proposal beacon script redeemer
```bash
cardano-options redeemers proposal-script manage-proposals \
  --out-file proposal_redeemer.json
```

##### Creating the required ProposalDatum
```bash
cardano-options datums proposal \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --quantity 10 \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --premium-asset 'lovelace' \
  --deposit 5000000 \
  --payment-address "$(cat personal.addr)" \
  --premium 2000000 \
  --strike-price "1 / 1" \
  --expiration 1714755185000 \
  --premium 3000000 \
  --strike-price "1 / 2" \
  --expiration 1714756185000 \
  --out-file proposal_datum.json
```

`quantity` is the number of units of the `offer-asset` that will be set aside for this contract.

`deposit` is the amount of ada the writer used for this minUTxOValue.

`premium`, `strike-price`, and `expiration` **must** be paired up like in the above example. These
are the possible terms the buyer can pick from when purchasing the options contract. The
`strike-price` can either be set to a fraction (like above) or a decimal like `0.1`. However, due to
the imprecision of floating point numbers, decimals will not be as precise as fractions; when
precision is needed, use a fraction. `expiration` must be in POSIXTime in milliseconds.

##### Building the transaction
The invalid-hereafter bound must be set to the earliest expiration among all of the Proposal UTxO's
`possibleTerms`. You can use `cardano-options convert-time` CLI to convert between POSIXTime and
slot numbers.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/create-proposal.sh).

## Closing a Proposal UTxO

Closing a Proposal UTxO requires the following steps:
1. Calculate the hash of the writer's staking credential.
2. Create the required spending script redeemer.
3. Calculate the required proposal beacon names to burn.
4. Create the required proposal beacon script redeemer.
5. Submit the transaction.

##### Calculate the hash of the staking credential used in the writer address
```bash
writerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file writer_stake.vkey)
```

##### Calculate the required beacon names to burn
Proposal UTxOs have four proposal beacons: 
- Offer beacon
- Ask beacon
- Premium beacon
- Trading pair beacon

```bash
## Get the proposal beacon policy id.
beaconPolicyId=$(cardano-options beacon-name policy-id \
  --proposal-beacons \
  --stdout) 

## Get the required beacon names.
offerBeaconName=$(cardano-options beacon-name asset-name offer-beacon \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --stdout)
askBeaconName=$(cardano-options beacon-name asset-name ask-beacon \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --stdout)
premiumBeaconName=$(cardano-options beacon-name asset-name premium-beacon \
  --premium-asset 'lovelace' \
  --stdout)
pairBeaconName=$(cardano-options beacon-name asset-name trading-pair-beacon \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --stdout)

offerBeacon="${beaconPolicyId}.${offerBeaconName}"
askBeacon="${beaconPolicyId}.${askBeaconName}"
premiumBeacon="${beaconPolicyId}.${premiumBeaconName}"
pairBeacon="${beaconPolicyId}.${pairBeaconName}"
```

##### Create the required redeemers
```bash
cardano-options redeemers proposal-script manage-proposals \
  --out-file proposal_redeemer.json

cardano-options redeemers options-script manage-proposal \
  --out-file options_redeemer.json
```

Closing proposal UTxOs requires using the `CreateCloseOrUpdateProposals` proposal beacon redeemer.

##### Building the transaction
The writer's staking credential must approve the transaction.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/close-proposal.sh).

## Updating Proposal UTxOs

The steps to update Proposal UTxOs are exactly the same as closing them, except you will need to
create the new outputs. All redeemers are the same.

When building the transaction, you will also need to specify the invalid-hereafter bound, and set it
to the earliest expiration among your new ProposalDatums.

By cross-referencing the [creation script](scripts/local-node/create-proposal.sh) and the [closing
script](scripts/local-node/close-proposal.sh), you can easily come up with your own update script.

## Purchasing a Proposal UTxO

Purchasing a Proposal UTxO requires the following steps:
1. Create the required spending script redeemer.
2. Create the new ActiveDatum for each proposal purchased.
3. Calculate the required proposal beacon names to burn.
4. Calculate the required active beacon names to mint.
5. Create the required proposal beacon script redeemer.
6. Create the required active beacon script redeemer.
7. Create the required datum for the premium payments to the writers.
8. Submit the transaction.

##### Create the required spending script redeemer
```bash
cardano-options redeemers options-script purchase \
  --desired-terms-index 0 \
  --out-file options_redeemer.json
```

`desired-terms-index` is the 0-based index for the terms in the proposal datum's `possibleTerms` you
would like to purchase.

##### Calculate the required beacon names to burn

If you are executing the proposal beacon script using `BurnProposalBeacons`, then all proposal
beacons attached to the Proposal UTxOs must be burned. If you are using
`CreateCloseOrUpdateProposals` instead, you only need to burn the proposal beacons that are not
needed for new Proposal UTxO outputs.

```bash
## Get the proposal beacon policy id.
proposalBeaconPolicyId=$(cardano-options beacon-name policy-id \
  --proposal-beacons \
  --stdout) 

## Get the required beacon names.
offerBeaconName=$(cardano-options beacon-name asset-name offer-beacon \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --stdout)
askBeaconName=$(cardano-options beacon-name asset-name ask-beacon \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --stdout)
premiumBeaconName=$(cardano-options beacon-name asset-name premium-beacon \
  --premium-asset 'lovelace' \
  --stdout)
pairBeaconName=$(cardano-options beacon-name asset-name trading-pair-beacon \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --stdout)

proposalOfferBeacon="${proposalBeaconPolicyId}.${offerBeaconName}"
proposalAskBeacon="${proposalBeaconPolicyId}.${askBeaconName}"
proposalPremiumBeacon="${proposalBeaconPolicyId}.${premiumBeaconName}"
proposalPairBeacon="${proposalBeaconPolicyId}.${pairBeaconName}"
```

##### Calculate the required beacon names to mint

Active UTxOs require four proposal beacons: 
- Offer beacon
- Ask beacon
- Contract ID beacon
- Trading pair beacon

```bash
activeBeaconPolicyId=$(cardano-options beacon-name policy-id \
  --active-beacons \
  --stdout) 

contractIdName=$(cardano-options beacon-name asset-name contract-id \
  --proposal-ref "7a1d3bb453b1ea065f391529d1a56e7e02bc03f4dc802acf3a1967dc447c3259#0" \
  --stdout)

activeOfferBeacon="${activeBeaconPolicyId}.${offerBeaconName}"
activeAskBeacon="${activeBeaconPolicyId}.${askBeaconName}"
activePairBeacon="${activeBeaconPolicyId}.${pairBeaconName}"
activeContractId="${activeBeaconPolicyId}.${contractIdName}"
```

##### Creating the required ActiveDatum
There are two ways to create the required ActiveDatum: manually or automatically.

You can create the ActiveDatum automatically with this command:
```bash
cardano-options datums active new auto \
  --testnet \
  --proposal-ref "7a1d3bb453b1ea065f391529d1a56e7e02bc03f4dc802acf3a1967dc447c3259#0" \
  --desired-terms-index 0 \
  --out-file active_datum.json
```

This command will query Koios to find that Proposal UTxO. It will create the new ActiveDatum based on
the Proposal UTxO's ProposalDatum.

If you would like to create the ActiveDatum manually (without requiring internet), you can do so 
with this command:

```bash
cardano-options datums active new manual \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --quantity 10 \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --deposit 5000000 \
  --payment-address "$(cat personal.addr)" \
  --strike-price "1 / 1" \
  --expiration 1714755185000 \
  --proposal-ref "7a1d3bb453b1ea065f391529d1a56e7e02bc03f4dc802acf3a1967dc447c3259#0" \
  --out-file active_datum.json
```

*All fields must match the ProposalDatum exactly*. While the fields that accept fractions can also
accept decimals, it is more accurate to use fractions directly. Converting decimals to fractions is
subject to the imprecision of floating-point numbers.

##### Create the required redeemers.
```bash
cardano-options redeemers proposal-script burn-all \
  --out-file proposal_redeemer.json

cardano-options redeemers active-script main \
  --out-file active_redeemer.json

cardano-options redeemers options-script purchase \
  --desired-terms-index 0 \
  --out-file options_redeemer.json
```

##### Create the premium payment datum.
```bash
cardano-options datums payment \
  --contract-id $contractIdName \
  --out-file payment_datum.json
```

##### Building the transaction
Make sure to keep the Key NFT for each proposal purchased!

To see how to build the transaction using a local node, refer
[here](scripts/local-node/purchase-proposal.sh).

## Updating a Payment Address

Updating the payment address requires the following steps:
1. Calculate the writer's staking credential hash.
2. Create the required spending script redeemer.
3. Create the required observer script redeemer.
4. Create the post-update ActiveDatum for each update made.
5. Create the required staking address for the address update observer script.
6. Submit the transaction.

##### Calculate the hash of the staking credential used in the writer address
```bash
writerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file writer_stake.vkey)
```

##### Create the address update observer's staking address
This address is needed to execute the script as a staking script.

```bash
cardano-options scripts \
  --address-update-script \
  --out-file address_observer.plutus

## Build the observer script's stake address.
observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file address_observer.plutus)
```

##### Create the required redeemers
```bash
cardano-options redeemers address-update-script observe-address-update \
  --out-file observer_redeemer.json

cardano-options redeemers options-script update-payment-address \
  --payment-address "$(cat personal2.addr)" \
  --deposit-increase 1000000 \
  --out-file options_redeemer.json
```

The `payment-address` field should be set to what the new address will be.

The `deposit-increase` field should specify the amount of additional ADA that was added to the UTxO
for the new minUTxOValue.

##### Creating the required ActiveDatum
There are two ways to create the required ActiveDatum: manually or automatically.

You can create the ActiveDatum automatically with this command:
```bash
cardano-options datums active post-address-update auto \
  --testnet \
  --contract-ref "5d6f286fb8f2fc173e9196d29ca92baa26a79a8301590789a36c5141687c22aa#1" \
  --payment-address "$(cat personal2.addr)" \
  --deposit-increase 1000000 \
  --out-file active_datum.json
```

This command will query Koios to find that Acttive UTxO. It will create the new ActiveDatum based on
the original UTxO's ActiveDatum.

If you would like to create the ActiveDatum manually (without requiring internet), you can do so 
with this command:

```bash
cardano-options datums active post-address-update manual \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --quantity 10 \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --deposit 5000000 \
  --payment-address "$(cat personal2.addr)" \
  --strike-price "1 / 1" \
  --expiration 1714755185000 \
  --contract-id "522c821f25a8feca8cfcfc9711c9cf2eb9ef470a0efaf4bc49c89c53bdb5b576" \
  --out-file active_datum.json
```

The `deposit` field should be the original value + the `deposit-increase` used with the spending
redeemer.

The `payment-address` field should match the address used for the spending redeemer.

*All other fields must match the original datum exactly*. While the fields that accept fractions can also
accept decimals, it is more accurate to use fractions directly. Converting decimals to fractions is
subject to the imprecision of floating-point numbers.

##### Building the transaction
The writer's staking credential must approve the transaction.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/update-payment-address.sh). 

## Executing an Active UTxO

Executing an Active UTxO requires the following steps:
1. Calculate the invalid-hereafter slot number based on the contract's expiration.
2. Create the required spending script redeemer.
3. Create the required active beacon script redeemer.
4. Create the required beacon names to burn.
5. Create the payment datum for the output to the payment address.
8. Submit the transaction.

##### Calculate the slot number associated with the expiration
```bash
expirationSlot=$(cardano-options convert-time --posix-time 1714755185000 --testnet)
```

##### Create the required redeemers
```bash
cardano-options redeemers active-script main \
  --out-file active_redeemer.json

cardano-options redeemers options-script execute \
  --out-file options_redeemer.json
```

##### Calculate the required beacons to burn

All active beacons attached to the active contracts must be burned as well as the Key NFT for each
contract being executed.

```bash
## Get the active beacon policy id.
activeBeaconPolicyId=$(cardano-options beacon-name policy-id \
  --active-beacons \
  --stdout) 

offerBeaconName=$(cardano-options beacon-name asset-name offer-beacon \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --stdout)
askBeaconName=$(cardano-options beacon-name asset-name ask-beacon \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --stdout)
pairBeaconName=$(cardano-options beacon-name asset-name trading-pair-beacon \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --stdout)
contractIdName="522c821f25a8feca8cfcfc9711c9cf2eb9ef470a0efaf4bc49c89c53bdb5b576"

activeOfferBeacon="${activeBeaconPolicyId}.${offerBeaconName}"
activeAskBeacon="${activeBeaconPolicyId}.${askBeaconName}"
activePairBeacon="${activeBeaconPolicyId}.${pairBeaconName}"
activeContractId="${activeBeaconPolicyId}.${contractIdName}"
```

##### Create the required payment datum for the payment output
```bash
cardano-options datums payment \
  --contract-id $contractIdName \
  --out-file payment_datum.json
```

This datum must be stored at the required payment address with the required amount of the ask asset.

##### Building the transaction
You need to set the invalid-hereafter of this transaction to the `$expirationSlot` variable. If you
are executing multiple contracts in a single transaction, the earliest expiration should be used.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/execute-contract.sh). 

## Closing an Expired Active UTxO

Closing an expired Active UTxO requires the following steps:
1. Calculate the invalid-before slot number based on the contract's expiration.
2. Calculate the writer's staking credential hash.
3. Create the required spending script redeemer.
4. Create the required active beacon script redeemer.
5. Create the required beacon names to burn.
6. Submit the transaction.

##### Calculate the slot number associated with the expiration
```bash
expirationSlot=$(cardano-options convert-time --posix-time 1714755185000 --testnet)
```

##### Calculate the hash of the staking credential used in the writer address
```bash
writerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file writer_stake.vkey)
```

##### Create the required redeemers
```bash
cardano-options redeemers active-script main \
  --out-file active_redeemer.json

cardano-options redeemers options-script close-expired \
  --out-file options_redeemer.json
```

##### Calculate the required beacons to burn

All active beacons attached to the active contracts must be burned.

```bash
## Get the active beacon policy id.
activeBeaconPolicyId=$(cardano-options beacon-name policy-id \
  --active-beacons \
  --stdout) 

offerBeaconName=$(cardano-options beacon-name asset-name offer-beacon \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --stdout)
askBeaconName=$(cardano-options beacon-name asset-name ask-beacon \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --stdout)
pairBeaconName=$(cardano-options beacon-name asset-name trading-pair-beacon \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --stdout)
contractIdName="522c821f25a8feca8cfcfc9711c9cf2eb9ef470a0efaf4bc49c89c53bdb5b576"

activeOfferBeacon="${activeBeaconPolicyId}.${offerBeaconName}"
activeAskBeacon="${activeBeaconPolicyId}.${askBeaconName}"
activePairBeacon="${activeBeaconPolicyId}.${pairBeaconName}"
activeContractId="${activeBeaconPolicyId}.${contractIdName}"
```

##### Building the transaction
You need to set the invalid-before of this transaction to the `$expirationSlot` variable. If you are
closing multiple expired contracts, the latest expiration should be used.

The writer's staking credential must approve the transaction.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/close-expired.sh). 

## Time Conversions
Since plutus scripts use POSIX time (in milliseconds) while cardano-cli uses slot numbers for the
transaction validity intervals, you need a way to convert between the two units.

``` Bash
cardano-options convert-time --testnet --slot 26668590

cardano-options convert-time --testnet --posix-time 1682351790000
```

## Queries

- All queries use Koios.
- All query commands are capable of saving results to a file or printing to stdout. 
- Results can be formatted as JSON, pretty, or plain. 

The pretty and plain formats are meant for printing to the stdout, but both can also be saved to a
file. The only difference between the pretty format and the plain format is the pretty format uses
ansii escape sequences to highlight certain items with color. The plain format is there as a
fallback in case the ansii escape sequences are causing issues for a user.

The JSON response to stdout can be directly piped into `jq` for a more human-friendly format.

> Note: Currently, the `cardano-options` CLI will only get the first 1000 UTxOs that satisfy a
> query. This could be 1000 personal UTxOs or 1000 contract UTxOs, depending on the query. For the
> beta release, 1000 should be plenty. The CLI will be expanded in the future to remove this cap.

### Querying Personal Addresses

In order to facilitate the use of remote nodes, `cardano-options` is capable of querying personal
addresses.

The command is simply:
```bash
cardano-options query personal-address \
  --testnet \
  --address $(cat personal.addr) \
  --pretty \
  --stdout
```

The UTxOs will be returned in lexicographical order based on the transaction hash.

For the pretty and plain formats, UTxOs that contain a reference script and/or a datum will show the
script hash or datum hash, respectively. For the pretty format, each type of hash will have a color
associated with it: Blue for script hashes and Green for datum hashes. UTxO assets are always shown.

There is an optional `--keys` flag. If used, it will only return UTxOs that contain Key NFTs for
Cardano-Options. This could be helpful for managing these Key NFTs.

This query can also work on plutus script addresses.

### Querying Proposal UTxOs

All possible queries for Proposal UTxOs are organized under the `cardano-options query proposals`
command.

If a target DApp address is not specified, then at least one asset filter must be used. It is not
possible to query *all* Proposal UTxOs. If you believe this would be a useful feature, feel free to
open an issue.

Filters can be combined to create more complicated filters. For example, this will only return
Proposal UTxOs using the specified assets.

```bash
cardano-options query proposals \
  --testnet \
  --stdout \
  --pretty \
  --premium-asset 'lovelace' \
  --offer-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --ask-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'
```

### Querying Active UTxOs

All possible queries for Active UTxOs are organized under the `cardano-options query actives` command.

If a target DApp address is not specified, then at least one asset filter must be used. It is not
possible to query *all* Active UTxOs. If you believe this would be a useful feature, feel free to
open an issue.

Filters can be combined to create more complicated filters. 

The `contract-id` field is optional. If you use it, the query will only return the Active UTxO with that
Contract ID. When this field is used, there can never be more than one result. It can return nothing if
there is no Active UTxO with that Contract ID.

### Querying the Current Time

Certain actions require knowing the current time. You can query the most recent slot number using
the `cardano-options query current-slot` command.
