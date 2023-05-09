#!/bin/sh

# A helper script for showing how to create an Assets UTxO to back a possible contract.

## Variables
dir="../assets/options-files/"
tmpDir="../assets/tmp/"

optionsScriptFile="${dir}options.plutus"
beaconPolicyFile="${dir}beacons.plutus"

optionsAddrFile="${dir}options.addr"

assetsDatumFile="${dir}assetsDatum.json"

beaconRedeemerFile="${dir}mintAssets.json"

assetsTokenName="417373657473" # This is the hexidecimal encoding for 'Assets'.

writerStakingPubKeyFile="../assets/wallets/01Stake.vkey"

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the writer..."
writerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $writerStakingPubKeyFile)

## Export the options validator script.
echo "Exporting the options validator script..."
cardano-options export-script options-script \
  --out-file $optionsScriptFile

## Create the options address.
echo "Creating the writer's options address..."
cardano-cli address build \
  --payment-script-file $optionsScriptFile \
  --stake-verification-key-file $writerStakingPubKeyFile \
  --testnet-magic 1 \
  --out-file $optionsAddrFile

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

## Create the minting redeemer.
echo "Creating the minting redeemer..."
cardano-options beacon-redeemer mint-assets \
  --out-file $beaconRedeemerFile

## Helper Assets beacon variable
assetsBeacon="${beaconPolicyId}.${assetsTokenName}"

## Create the datum.
echo "Creating the datum..."
cardano-options options-datum assets-datum \
  --beacon-policy-id $beaconPolicyId \
  --current-asset-is-lovelace \
  --quantity 10000000 \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --out-file $assetsDatumFile

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in fd0bbef32859abf1d61c71d54c6b2416ad664e2c978581e4df50fc2f795b2e20#3 \
  --tx-out "$(cat ${optionsAddrFile}) + 15000000 lovelace + 1 ${assetsBeacon}" \
  --tx-out-inline-datum-file $assetsDatumFile \
  --mint "1 ${assetsBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --required-signer-hash $writerPubKeyHash \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
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