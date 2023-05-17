#!/bin/sh

# A helper script for showing how to purchase an options contract.

## Variables
dir="../assets/options-files/"
tmpDir="../assets/tmp/"

optionsScriptFile="${dir}options.plutus"
beaconPolicyFile="${dir}beacons.plutus"

beaconRedeemerFile="${dir}burnBeacons.json"
spendingRedeemerFile="${dir}executeContract.json"

activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'

contractId="eda1ae16877891c958f5ca2de23867cbf03ca6d72c48586468ccd536deb26b3e"

## Export the options validator script.
echo "Exporting the options validator script..."
cardano-options export-script options-script \
  --out-file $optionsScriptFile

## Export the beacon policy for that trading pair.
echo "Exporting the beacon policy script..."
cardano-options export-script beacon-policy \
  --current-asset-is-lovelace \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --out-file $beaconPolicyFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile) 

## Create the burning redeemer.
echo "Creating the burning redeemer..."
cardano-options beacon-redeemer burn-beacons \
  --out-file $beaconRedeemerFile

## Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-options options-redeemer execute-contract \
  --out-file $spendingRedeemerFile

## Create the writer's bech32 address.
echo "Creating the writer's bech32 address..."
writerAddr=$(cardano-options convert-address \
  --payment-pubkey-hash "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f" \
  --staking-pubkey-hash "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa" \
  --stdout)

## Helper beacon variables
activeBeacon="${beaconPolicyId}.${activeTokenName}"
contractIDBeacon="${beaconPolicyId}.${contractId}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in ba11ed4cd3c95c6774952fff3f841ba630773c1529ed605932ed5b5124bc9c53#3 \
  --tx-in ba11ed4cd3c95c6774952fff3f841ba630773c1529ed605932ed5b5124bc9c53#2 \
  --tx-in 1c2798410ce33f1d6efc473d9a4971bc002a2846681255d8fd2e04d61d7e54aa#1 \
  --tx-in 2a67cc926e38242e98211be24796c70401bcd72dab1a2204aa49b33d7990908c#0 \
  --tx-in-script-file $optionsScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $spendingRedeemerFile \
  --tx-out "${writerAddr} + 5000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a + 1 ${contractIDBeacon}" \
  --mint "-1 ${activeBeacon} + -1 ${contractIDBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address "$(cat ../assets/wallets/02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --invalid-hereafter 28654389 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"