#!/bin/sh

# A helper script for showing how to update the address of an active contract's ActiveDatum UTxOs.

## Variables
dir="../assets/options-files/"
tmpDir="../assets/tmp/"

optionsScriptFile="${dir}options.plutus"
beaconPolicyFile="${dir}beacons.plutus"

optionsAddrFile="${dir}options.addr"

spendingRedeemerFile="${dir}updateAddress.json"

newDatumFile="${dir}active.json"

activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'

contractId="eda1ae16877891c958f5ca2de23867cbf03ca6d72c48586468ccd536deb26b3e"

writerStakingPubKeyFile="../assets/wallets/01Stake.vkey"

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the writer..."
writerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $writerStakingPubKeyFile)

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

## Helper beacon variables
activeBeacon="${beaconPolicyId}.${activeTokenName}"
contractIDBeacon="${beaconPolicyId}.${contractId}"

## Create the new datum for the active contract.
echo "Creating the datum..."
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
  --contract-id $contractId \
  --out-file $newDatumFile

## Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-options options-redeemer update-address \
  --payment-pubkey-hash "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f" \
  --staking-pubkey-hash "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa" \
  --out-file $spendingRedeemerFile

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 04bdd49521adb7b313e4eb3588169e883f91f82a1f8f3bccaefdb48d647baa3f#3 \
  --tx-in ba11ed4cd3c95c6774952fff3f841ba630773c1529ed605932ed5b5124bc9c53#0 \
  --tx-in-script-file $optionsScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $spendingRedeemerFile \
  --tx-out "$(cat $optionsAddrFile) + 15000000 lovelace + 1 ${activeBeacon} + 1 ${contractIDBeacon}" \
  --tx-out-inline-datum-file $newDatumFile \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --required-signer-hash $writerPubKeyHash \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --signing-key-file ../assets/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"