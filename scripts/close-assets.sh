#!/bin/sh

# A helper script for showing how to close an Assets UTxO.

## Variables
dir="../assets/options-files/"
tmpDir="../assets/tmp/"

optionsScriptFile="${dir}options.plutus"
beaconPolicyFile="${dir}beacons.plutus"

beaconRedeemerFile="${dir}burnBeacons.json"
spendingRedeemerFile="${dir}closeAssets.json"

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
cardano-options options-redeemer close-assets \
  --out-file $spendingRedeemerFile

## Helper Assets beacon variable
assetsBeacon="${beaconPolicyId}.${assetsTokenName}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 07867c05d9d11c078b0193cd01f62a443962b474010f0e4b260840f6c832a921#0 \
  --tx-in-script-file $optionsScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $spendingRedeemerFile \
  --mint "-1 ${assetsBeacon}" \
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