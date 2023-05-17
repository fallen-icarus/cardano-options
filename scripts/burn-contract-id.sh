#!/bin/sh

# A helper script for showing how to burn a contractID for an expired contract.

## Variables
dir="../assets/options-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

beaconRedeemerFile="${dir}burnBeacons.json"

contractId="eda1ae16877891c958f5ca2de23867cbf03ca6d72c48586468ccd536deb26b3e"

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

## Helper beacon variable
contractIDBeacon="${beaconPolicyId}.${contractId}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 996a030783d8fe22d8698e56ccccec43e0ac45662b6cbfddb834914dc0bf07d9#0 \
  --tx-out "$(cat ../assets/wallets/01.addr) + 2000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --mint "-1 ${contractIDBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"