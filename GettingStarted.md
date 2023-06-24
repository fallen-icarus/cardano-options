# Getting Started

:warning: Assumes a local PreProduction Testnet node running locally and `cardano-cli` installed since it is used to actually build and sign transactions.

Template bash scripts that follow these steps are available [here](scripts/).

When integration testing, it is highly recommended that you change the string passed to the mkBeaconPolicy function [here](src/CardanoOptions.hs#L197). When developers make mistakes (myself included), it can create bad/locked utxos that will appear when you query the beacons. This can complicate your own testing. To avoid this, this extra parameter was added. Change the string to something unique to you. You should remember to change it to the desired string for mainnet.

**Make this change before building the executable in the next section.**

The `cardano-options` CLI uses the Blockfrost API endpoints for the Preproduction Testnet (Koios support will be added in a future version). You will need an api key to query the beacon tokens. You can go [here](https://blockfrost.io/#pricing) to get one for free; only an email address is required.

If a specific beacon token has never been minted before, querying the Blockfrost endpoints will return "The requested component has not been found." This is due to the beacon name being part of the Blockfrost api url like:

``` Url
https://cardano-preprod.blockfrost.io/api/v0/assets/{beacon_name}/addresses
```

If the beacon has not been minted before, this URL does not exist yet. Once the beacon is minted, the URL is generated. If the beacons have been minted before but there are currently no beacons in circulation, then the API will return an empty list.

---
## Table of Contents
- [Installing](#installing)
- [Minting Test Tokens](#minting-test-tokens)
- [Convert POSIX time <--> Slot](#convert-posix-time----slot)
- [Address Conversions](#address-conversions)
- [Create An Asset UTxO](#create-an-asset-utxo)
- [Close Asset UTxOs](#close-asset-utxos)
- [Create Proposal UTxOs](#create-proposal-utxos)
- [Close Proposal UTxOs](#close-proposal-utxos)
- [Purchase Contract](#purchase-contract)
- [Execute Contract](#execute-contract)
- [Close Expired Contract](#close-an-expired-contract)
- [Update Payment Address in Contract](#update-payment-address-in-an-active-contract)
- [Burn a ContractID](#burn-a-contractid)
- [Query Beacons](#query-beacons)

---
## Installing

### Using Cabal - RECOMMENDED

#### Install the necessary packages - similar to cardano-node
```
sudo apt update
sudo apt upgrade
sudo apt-get install autoconf automake build-essential curl g++ git jq libffi-dev libgmp-dev libncursesw5 libssl-dev libsystemd-dev libtinfo-dev libtool make pkg-config wget zlib1g-dev liblzma-dev libpq-dev
```

#### Install libsodium and scep256k1
```
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install

cd ../
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout ac83be33
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
make check
sudo make install
sudo ldconfig
```

Add the following lines to your `$HOME/.bashrc` file:
```
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
```

#### Install GHC 8.10.7 and cabal
```
cd
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
Make sure to install the required packages it mentions before hitting ENTER.

Prepend or append the required PATH variable.

You do not need to install the haskell-langauge-server.

You do not need to install stack.

Press ENTER to proceed.
```
source .bashrc
ghcup install ghc 8.10.7
ghcup set ghc 8.10.7
```

#### Build the executable
```
git clone https://github.com/fallen-icarus/cardano-options
cd cardano-options
cabal clean
cabal update
cabal build all
```

The `cardano-options` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-options-0.1.0.0/x/cardano-options/build/cardano-options/cardano-options`. Move the program to somewhere in your `$PATH`.

All `cardano-options` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

### Using Nix
The [Nix Package Manager](https://nixos.org/) can be installed on most Linux distributions by downloading and running the installation script
```
curl -L https://nixos.org/nix/install > install-nix.sh
chmod +x install-nix.sh
./install-nix.sh
```
and following the directions.

#### Configuring the Binary Caches
While this step is optional, it can save several hours of time since nix will need a copy of every necessary package. Therefore, it is highly recommended that you do this.
```
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee -a /etc/nix/nix.conf
experimental-features = nix-command flakes
allow-import-from-derivation = true
substituters = https://cache.nixos.org https://cache.iog.io https://cache.zw3rk.com
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=
EOF
```
The caches used here come from the plutus-apps contributing [doc](https://github.com/input-output-hk/plutus-apps/blob/713955dea45739de6df3c388717123cfec648914/CONTRIBUTING.adoc#how-to-get-a-shell-environment-with-tools).

You will need to restart the nix service in order to make sure that it uses the newly configured caches. A sure fire way to do this is restart your machine.

#### Building the Executable
```
git clone https://github.com/fallen-icarus/cardano-options
git clone https://github.com/input-output-hk/plutus-apps
cd plutus-apps
git checkout 68c3721
nix develop # This step can take an hour even with the caches configured
# Set accept-flake-config to true and permanently mark the value as trusted
```
The last command should drop you into a nix terminal once it is finished running. Execute the following within the nix terminal.
```
cd ../cardano-options
cabal clean
cabal update
cabal build all
```

If all goes well, the `cardano-options` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-options-0.1.0.0/x/cardano-options/build/cardano-options/cardano-options`. Move the program to somewhere in your $PATH.

You can now exit the nix terminal with `exit`.

All `cardano-options` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

#### Troubleshooting Nix
If you encounter a libsodium error, you may need to first install libsodium separately. While not inside the nix terminal (you can leave with `exit`), execute the following:
```
cd # return to your home directory
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install
```
Once installed, you can retry the build after exporting the following variables while inside the nix terminal:
```
cd ../plutus-apps
nix develop # This should only take a minute this time
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
cabal build all
```


--- 
## Minting test tokens
An always succeeding minting policy as well as the required redeemer are included [here](scripts/mint-test-tokens/). In that directory is also the template bash script that uses them. These can be used to create as many native tokens as needed to test this lending dApp.

---
## Convert POSIX time <--> Slot
``` Bash
cardano-loans convert-time --slot 26668590

cardano-loans convert-time --posix-time 1682351790000
```

---
## Address Conversions
Since plutus smart contracts do not use bech32 encoded addresses while cardano-cli does, addresses will need to be converted as necessary. To make this as painless as possible, `cardano-options` is capable of doing these conversions for you. It uses [`cardano-addresses`](https://github.com/input-output-hk/cardano-addresses) under the hood.

### Plutus Hashes to Bech32
``` Bash
cardano-options convert-address \
  --payment-pubkey-hash ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2 \
  --staking-pubkey-hash 623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c \
  --stdout
```

All bech32 addresses generated with this command will be for the preproduction testnet. When the protocol is ready for mainnet, support will be added for mainnet addresses.

### Bech32 to Plutus Hashes
``` Bash
cardano-options convert-address \
  --address addr_test1vrlfp27zjnjlsak5f7dnjkpl9ekeq5ezc3e4uw769y5rgtc4qvv2f \
  --stdout
```

This will result in the following output when piped to `jq`:
``` JSON
{
  "network_tag": 0,
  "payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
  "payment_script_hash": null,
  "staking_pubkey_hash": null,
  "staking_script_hash": null
}
```

The `network_tag` of 0 corresponds to the Preproduction testnet (1 would be Mainnet). This address uses a spending pubkey and has no staking credential.

---
## Create an Asset UTxO

#### Calculate the staking pubkey hash for the writer.
``` Bash
writerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file writeStake.vkey)
```

#### Export the Options validator script.
``` Bash
cardano-options export-script options-script \
  --out-file options.plutus
```

#### Create the writer's option address.
``` Bash
cardano-cli address build \
  --payment-script-file options.plutus \
  --stake-verification-key-file writeStake.vkey \
  --testnet-magic 1 \
  --out-file writerOption.addr
```

#### Export the beacon policy script for that trading pair.
``` Bash
cardano-options export-script beacon-policy \
  --current-asset-is-lovelace \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --out-file beacons.plutus
```

#### Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the minting redeemer.
``` Bash
cardano-options beacon-redeemer mint-assets \
  --out-file mintAsset.json
```

#### Create the AssetDatum.
``` Bash
cardano-options options-datum assets-datum \
  --beacon-policy-id $beaconPolicyId \
  --current-asset-is-lovelace \
  --quantity 10000000 \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --out-file assetDatum.json
```
#### Create a helper beacon variable.
``` Bash
assetsBeacon="${beaconPolicyId}.417373657473"
```

#### Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_with_assets_and_fee> \
  --tx-out "$(cat writerOption.addr) + 15000000 lovelace + 1 ${assetsBeacon}" \
  --tx-out-inline-datum-file assetDatum.json \
  --mint "1 ${assetsBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file mintAsset.json \
  --required-signer-hash $writerPubKeyHash \
  --change-address "$(cat <personal_address>)" \
  --tx-in-collateral <collateral_utxo_from_personal_address> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file writerPayment.skey \
  --signing-key-file writerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Close Asset UTxOs

It is possible to close multiple Asset UTxOs in a single transaction.

#### Calculate the staking pubkey hash for the writer.
``` Bash
writerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file writeStake.vkey)
```

#### Export the Options validator script.
``` Bash
cardano-options export-script options-script \
  --out-file options.plutus
```

#### Export the beacon policy script for that trading pair.
``` Bash
cardano-options export-script beacon-policy \
  --current-asset-is-lovelace \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --out-file beacons.plutus
```

#### Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the burning redeemer.
``` Bash
cardano-options beacon-redeemer burn-beacons \
  --out-file burn.json
```

#### Create the spending redeemer.
``` Bash
cardano-options options-redeemer close-assets \
  --out-file closeAssets.json
```

#### Create a helper beacon variable.
``` Bash
assetsBeacon="${beaconPolicyId}.417373657473"
```

#### Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <asset_utxo_to_close> \
  --tx-in-script-file options.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file closeAssets.json \
  --mint "-1 ${assetsBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file burn.json \
  --required-signer-hash $writerPubKeyHash \
  --change-address "$(cat <personal_address>)" \
  --tx-in-collateral <collateral_utxo_from_personal_address> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file writerPayment.skey \
  --signing-key-file writeStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Create Proposal UTxOs

#### Calculate the staking pubkey hash for the writer.
``` Bash
writerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file writeStake.vkey)
```

#### Export the Options validator script.
``` Bash
cardano-options export-script options-script \
  --out-file options.plutus
```

#### Export the beacon policy script for that trading pair.
``` Bash
cardano-options export-script beacon-policy \
  --current-asset-is-lovelace \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --out-file beacons.plutus
```

#### Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the minting redeemer.
``` Bash
cardano-options beacon-redeemer mint-proposal \
  --out-file mintProposed.json
```

#### Create a helper beacon variable.
``` Bash
proposedBeacon="${beaconPolicyId}.50726f706f736564"
```

#### Create the ProposedDatum. Create as many as necessary.
``` Bash
cardano-options options-datum proposal-datum \
  --beacon-policy-id $beaconPolicyId \
  --current-asset-is-lovelace \
  --quantity 10000000 \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --strike-price-numerator 1 \
  --strike-price-denominator 1000000 \
  --payment-pubkey-hash "$(cat writerPersonal.pkh)" \
  --premium-asset-is-lovelace \
  --premium 2000000 \
  --expiration 26668590 \
  --out-file proposedDatum1.json
```

The above datum's writer address is just the writer's personal address without a staking credential. It is also possible to use an address with a staking credential. Execute `cardano-options options-datum proposal-datum --help` for details.

#### Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_for_min_deposit_and_fee> \
  --tx-out "$(cat writerOption.addr) + 3000000 lovelace + 1 ${proposedBeacon}" \
  --tx-out-inline-datum-file proposalDatum1.json \
  --mint "1 ${proposedBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file mintProposed.json \
  --required-signer-hash $writerPubKeyHash \
  --change-address "$(cat writerPersonal.addr)" \
  --tx-in-collateral <collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file writerPayment.skey \
  --signing-key-file writerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Close Proposal UTxOs

#### Calculate the staking pubkey hash for the writer.
``` Bash
writerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file writeStake.vkey)
```

#### Export the Options validator script.
``` Bash
cardano-options export-script options-script \
  --out-file options.plutus
```

#### Export the beacon policy script for that trading pair.
``` Bash
cardano-options export-script beacon-policy \
  --current-asset-is-lovelace \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --out-file beacons.plutus
```

#### Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the burning redeemer.
``` Bash
cardano-options beacon-redeemer burn-beacons \
  --out-file burn.json
```

#### Create the spending redeemer.
``` Bash
cardano-options options-redeemer close-proposal \
  --out-file closeProposed.json
```

#### Create helper Proposed beacon variable.
``` Bash
proposedBeacon="${beaconPolicyId}.50726f706f736564"
```

#### Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_to_close> \
  --tx-in-script-file options.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file closeProposed.json \
  --mint "-1 ${proposedBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file burn.json \
  --required-signer-hash $writerPubKeyHash \
  --change-address "$(cat <personal_address>)" \
  --tx-in-collateral <collateral_utxo_from_personal_address> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file writerPayment.skey \
  --signing-key-file writeStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Purchase Contract

In this version of the protocol, only one contract can be purchased per tx.

#### Export the Options validator script.
``` Bash
cardano-options export-script options-script \
  --out-file options.plutus
```

#### Export the beacon policy script for that trading pair.
``` Bash
cardano-options export-script beacon-policy \
  --current-asset-is-lovelace \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --out-file beacons.plutus
```

#### Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the minting redeemer.
``` Bash
cardano-options beacon-redeemer mint-active \
  --contract-id <tx_hash_of_the_asset_utxo_to_be_consumed> \
  --staking-pubkey-hash <staking_pubkey_hash_for_target_address> \
  --out-file mintActive.json
```

#### Create helper beacon variables.
``` Bash
assetsBeacon="${beaconPolicyId}.417373657473"
proposedBeacon="${beaconPolicyId}.50726f706f736564"
activeBeacon="${beaconPolicyId}.416374697665"
contractIDBeacon="${beaconPolicyId}.<tx_hash_of_the_asset_utxo_to_be_consumed>"
```

#### Create the datum for the accepted contract.
``` Bash
cardano-options options-datum active-datum \
  --beacon-policy-id $beaconPolicyId \
  --current-asset-is-lovelace \
  --quantity 10000000 \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --strike-price-numerator 1 \
  --strike-price-denominator 1000000 \
  --payment-pubkey-hash "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f" \
  --premium-asset-is-lovelace \
  --premium 2000000 \
  --expiration 28654389 \
  --contract-id <tx_hash_of_the_asset_utxo_to_be_consumed> \
  --out-file activeDatum.json
```

Make sure the terms match those in the proposed and asset datums otherwise the transaction will fail.

#### Create the spending redeemer.
``` Bash
cardano-options options-redeemer purchase-contract \
  --out-file purchase.json
```

#### Create the target address for the premium payment (from the proposed datum).
``` Bash
premiumAddr=$(cardano-options convert-address \
  --payment-pubkey-hash "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f" \
  --stdout)
```

#### Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <buyer_utxo_for_fee_and_premium> \
  --tx-in <asset_utxo> \
  --tx-in-script-file options.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file purchase.json \
  --tx-in <proposed_utxo> \
  --tx-in-script-file options.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file purchase.json \
  --tx-out "<options_address> + 15000000 lovelace + 1 ${activeBeacon} + 1 ${contractIDBeacon}" \
  --tx-out-inline-datum-file $acceptDatumFile \
  --tx-out "${premiumAddr} + 5000000 lovelace" \
  --tx-out "$(cat buyer.addr) + 2000000 lovelace + 1 ${contractIDBeacon}" \
  --mint "-1 ${assetsBeacon} + -1 ${proposedBeacon} + 1 ${activeBeacon} + 2 ${contractIDBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file mintActive.json \
  --change-address "$(cat buyer.addr)" \
  --tx-in-collateral <collateral_utxo_from_buyer> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file buyer.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Execute Contract

#### Export the Options validator script.
``` Bash
cardano-options export-script options-script \
  --out-file options.plutus
```

#### Export the beacon policy script for that trading pair.
``` Bash
cardano-options export-script beacon-policy \
  --current-asset-is-lovelace \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --out-file beacons.plutus
```

#### Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the burning redeemer.
``` Bash
cardano-options beacon-redeemer burn-beacons \
  --out-file burn.json
```

#### Create the spending redeemer.
``` Bash
cardano-options options-redeemer execute-contract \
  --out-file execute.json
```

#### Create the payment address where the desired asset will be sent (from the datum).
``` Bash
writerAddr=$(cardano-options convert-address \
  --payment-pubkey-hash "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f" \
  --staking-pubkey-hash "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa" \
  --stdout)
```

#### Create helper beacon variables.
``` Bash
activeBeacon="${beaconPolicyId}.416374697665"
contractIDBeacon="${beaconPolicyId}.<contract_id_to_execute>"
```

#### Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <contract_owner_utxo_with_fee_and_asset_to_swap> \
  --tx-in <active_utxo_for_target_contract> \
  --tx-in-script-file options.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file execute.json \
  --tx-out "${writerAddr} + 5000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a + 1 ${contractIDBeacon}" \
  --mint "-1 ${activeBeacon} + -1 ${contractIDBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file burn.json \
  --change-address "$(cat contractOwner.addr)" \
  --tx-in-collateral <collateral_utxo_from_contract_owner> \
  --invalid-hereafter 28654389 \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file contractOwner.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
#### Close an expired contract.

#### Calculate the staking pubkey hash for the writer.
``` Bash
writerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file writeStake.vkey)
```

#### Export the Options validator script.
``` Bash
cardano-options export-script options-script \
  --out-file options.plutus
```

#### Export the beacon policy script for that trading pair.
``` Bash
cardano-options export-script beacon-policy \
  --current-asset-is-lovelace \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --out-file beacons.plutus
```

#### Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the burning redeemer.
``` Bash
cardano-options beacon-redeemer burn-beacons \
  --out-file burn.json
```

#### Create the spending redeemer.
``` Bash
cardano-options options-redeemer close-expired-contract \
  --out-file closeExpired.json
```

#### Create helper beacon variables.
``` Bash
activeBeacon="${beaconPolicyId}.416374697665"
contractIDBeacon="${beaconPolicyId}.<contract_id_to_execute>"
```

#### Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <expired_utxo> \
  --tx-in-script-file options.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file closeExpired.json \
  --mint "-1 ${activeBeacon} + -1 ${contractIDBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file burn.json \
  --required-signer-hash $writerPubKeyHash \
  --change-address "$(cat writerPersonal.addr)" \
  --tx-in-collateral <collateral_utxo> \
  --invalid-before <contract_expiration_plus_one> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file writerPayment.skey \
  --signing-key-file writerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Update payment address in an active contract.

#### Export the Options validator script.
``` Bash
cardano-options export-script options-script \
  --out-file options.plutus
```

#### Export the beacon policy script for that trading pair.
``` Bash
cardano-options export-script beacon-policy \
  --current-asset-is-lovelace \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --out-file beacons.plutus
```

#### Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create helper beacon variables.
``` Bash
activeBeacon="${beaconPolicyId}.416374697665"
contractIDBeacon="${beaconPolicyId}.<contract_id_of_contract_to_update>"
```

#### Create the new ActiveDatum.
``` Bash
cardano-options options-datum active-datum \
  --beacon-policy-id $beaconPolicyId \
  --current-asset-is-lovelace \
  --quantity 10000000 \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --strike-price-numerator 1 \
  --strike-price-denominator 1000000 \
  --payment-pubkey-hash "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f" \
  --staking-pubkey-hash "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa" \
  --premium-asset-is-lovelace \
  --premium 2000000 \
  --expiration 28654389 \
  --contract-id <contract_id_of_contract_to_update> \
  --out-file updatedDatum.json
```

#### Create the spending redeemer.
``` Bash
cardano-options options-redeemer update-address \
  --payment-pubkey-hash "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f" \
  --staking-pubkey-hash "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa" \
  --out-file updateAddress.json
```

The address in the redeemer must match the address in the datum or else the transaction will fail.

#### Create and submit the transaction.
```Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_for_fee> \
  --tx-in <active_utxo> \
  --tx-in-script-file options.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file updateAddress.json \
  --tx-out "<options_address> + 15000000 lovelace + 1 ${activeBeacon} + 1 ${contractIDBeacon}" \
  --tx-out-inline-datum-file updatedDatum.json \
  --change-address "$(cat writerPersonal.addr)" \
  --tx-in-collateral <collateral_utxo> \
  --required-signer-hash $writerPubKeyHash \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file writerPayment.skey \
  --signing-key-file writeStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Burn a ContractID

#### Export the beacon policy script for that trading pair.
``` Bash
cardano-options export-script beacon-policy \
  --current-asset-is-lovelace \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --out-file beacons.plutus
```

#### Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the burning redeemer.
``` Bash
cardano-options beacon-redeemer burn-beacons \
  --out-file burn.json
```

#### Create a helper beacon variable.
``` Bash
contractIDBeacon="${beaconPolicyId}.<contract_id_to_burn>"
```

#### Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_wit_id_and_fee> \
  --mint "-1 ${contractIDBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file burn.json \
  --change-address "$(cat owner.addr)" \
  --tx-in-collateral <collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file owner.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Query Beacons

To see all queries supported, executed `cardano-options query --help`. Below are example responses from the queries.

#### Available Contracts
``` JSON
[
  {
    "address": "addr_test1zp9xg507keq0xueuxlsrz3tum0pjcayvuf5lg2064npmr7eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqvw5tff",
    "assets_utxo": {
      "info": {
        "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
        "current_asset": "lovelace",
        "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "quantity": 10000000
      },
      "output_index": "0",
      "tx_hash": "07867c05d9d11c078b0193cd01f62a443962b474010f0e4b260840f6c832a921",
      "utxo_assets": [
        {
          "asset": "lovelace",
          "quantity": 15000000
        },
        {
          "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.417373657473",
          "quantity": 1
        }
      ]
    },
    "proposed_utxos": [
      {
        "info": {
          "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
          "current_asset": "lovelace",
          "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          "expiration_slot": 27959085,
          "premium": 2000000,
          "premium_asset": "lovelace",
          "quantity": 10000000,
          "strike_price": {
            "denominator": 1000000,
            "numerator": 1
          },
          "writer_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
          "writer_address_payment_script_hash": null,
          "writer_address_staking_pubkey_hash": null,
          "writer_address_staking_script_hash": null
        },
        "output_index": "0",
        "tx_hash": "07e2cae3181f453804634460f0a1e95d864e17844bb4f67a64f8e9222b9e8190",
        "utxo_assets": [
          {
            "asset": "lovelace",
            "quantity": 3000000
          },
          {
            "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.50726f706f736564",
            "quantity": 1
          }
        ]
      },
      {
        "info": {
          "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
          "current_asset": "lovelace",
          "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          "expiration_slot": 27956085,
          "premium": 1500000,
          "premium_asset": "lovelace",
          "quantity": 10000000,
          "strike_price": {
            "denominator": 1000000,
            "numerator": 1
          },
          "writer_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
          "writer_address_payment_script_hash": null,
          "writer_address_staking_pubkey_hash": null,
          "writer_address_staking_script_hash": null
        },
        "output_index": "1",
        "tx_hash": "07e2cae3181f453804634460f0a1e95d864e17844bb4f67a64f8e9222b9e8190",
        "utxo_assets": [
          {
            "asset": "lovelace",
            "quantity": 3000000
          },
          {
            "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.50726f706f736564",
            "quantity": 1
          }
        ]
      }
    ]
  }
]
```

Only one available contract was found for this trading pair. It contains the Asset UTxO and a list of Proposed UTxOs that could be paired with it. This has all the necessary information for purchasing this contract from the writer. The `info` field is the datum for that UTxO.

#### Own Assets UTxOs
``` JSON
[
  {
    "info": {
      "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
      "current_asset": "lovelace",
      "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
      "quantity": 10000000
    },
    "output_index": "0",
    "tx_hash": "07867c05d9d11c078b0193cd01f62a443962b474010f0e4b260840f6c832a921",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 15000000
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.417373657473",
        "quantity": 1
      }
    ]
  }
]
```

This user only has one Asset UTxO at their address. The `info` field is the datum.

#### Own Proposals
``` JSON
[
  {
    "info": {
      "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
      "current_asset": "lovelace",
      "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
      "expiration_slot": 27959085,
      "premium": 2000000,
      "premium_asset": "lovelace",
      "quantity": 10000000,
      "strike_price": {
        "denominator": 1000000,
        "numerator": 1
      },
      "writer_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
      "writer_address_payment_script_hash": null,
      "writer_address_staking_pubkey_hash": null,
      "writer_address_staking_script_hash": null
    },
    "output_index": "0",
    "tx_hash": "07e2cae3181f453804634460f0a1e95d864e17844bb4f67a64f8e9222b9e8190",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 3000000
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.50726f706f736564",
        "quantity": 1
      }
    ]
  },
  {
    "info": {
      "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
      "current_asset": "lovelace",
      "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
      "expiration_slot": 27956085,
      "premium": 1500000,
      "premium_asset": "lovelace",
      "quantity": 10000000,
      "strike_price": {
        "denominator": 1000000,
        "numerator": 1
      },
      "writer_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
      "writer_address_payment_script_hash": null,
      "writer_address_staking_pubkey_hash": null,
      "writer_address_staking_script_hash": null
    },
    "output_index": "1",
    "tx_hash": "07e2cae3181f453804634460f0a1e95d864e17844bb4f67a64f8e9222b9e8190",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 3000000
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.50726f706f736564",
        "quantity": 1
      }
    ]
  }
]
```

This user has two Proposed UTxOs at their address. The `info` field has the datum for that UTxO.

#### Own Active UTxOs
``` JSON
[
  {
    "info": {
      "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
      "contract_id": "82653d2221932d287f69f5169b7b96e87866cceae698e4a4e8f4ba1b2030caa6",
      "current_asset": "lovelace",
      "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
      "expiration_slot": 27956685,
      "premium": 1500000,
      "premium_asset": "lovelace",
      "quantity": 10000000,
      "strike_price": {
        "denominator": 1000000,
        "numerator": 1
      },
      "writer_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
      "writer_address_payment_script_hash": null,
      "writer_address_staking_pubkey_hash": null,
      "writer_address_staking_script_hash": null
    },
    "output_index": "0",
    "tx_hash": "4eb9dcf443c069103a8e5682daa070817936942110ed8fdf204b1ff1e8234bf0",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 15000000
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.416374697665",
        "quantity": 1
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.82653d2221932d287f69f5169b7b96e87866cceae698e4a4e8f4ba1b2030caa6",
        "quantity": 1
      }
    ]
  }
]
```

This user currently has only one Active contract at their options address. The `info` field is the datum.

#### Specific Contract Info
``` JSON
[
  {
    "info": {
      "beacon_symbol": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d",
      "contract_id": "3c4d40a176b599ecb82d45a80795bb8219e1dcd4d84e0dde64f46ff872f7d52f",
      "current_asset": "lovelace",
      "desired_asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
      "expiration_slot": 27974054,
      "premium": 2000000,
      "premium_asset": "lovelace",
      "quantity": 10000000,
      "strike_price": {
        "denominator": 1000000,
        "numerator": 1
      },
      "writer_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
      "writer_address_payment_script_hash": null,
      "writer_address_staking_pubkey_hash": null,
      "writer_address_staking_script_hash": null
    },
    "output_index": "0",
    "tx_hash": "d6cc14ea86416fde3fba3626e75232c3bed0a445efd0ce8ebda99ac3cafa981d",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 15000000
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.3c4d40a176b599ecb82d45a80795bb8219e1dcd4d84e0dde64f46ff872f7d52f",
        "quantity": 1
      },
      {
        "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.416374697665",
        "quantity": 1
      }
    ]
  }
]
```

The information for the target contract was returned. The `info` field is the datum.

#### Own Contracts
Looking from the perspective of the user with the contractID key.

``` JSON
[
  {
    "asset": "7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d.82653d2221932d287f69f5169b7b96e87866cceae698e4a4e8f4ba1b2030caa6",
    "quantity": 1
  }
]
```