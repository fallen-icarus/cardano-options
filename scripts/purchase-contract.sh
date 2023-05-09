#!/bin/sh

# A helper script for showing how to purchase an options contract.

## Variables
dir="../assets/options-files/"
tmpDir="../assets/tmp/"

optionsScriptFile="${dir}options.plutus"
beaconPolicyFile="${dir}beacons.plutus"

optionsAddr="addr_test1zp9xg507keq0xueuxlsrz3tum0pjcayvuf5lg2064npmr7eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqvw5tff"

beaconRedeemerFile="${dir}mintActive.json"
spendingRedeemerFile="${dir}acceptContract.json"

acceptDatumFile="${dir}active.json"

assetsTokenName="417373657473" # This is the hexidecimal encoding for 'Assets'.
proposedTokenName="50726f706f736564" # This is the hexidecimal encoding for 'Proposed'.
activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'

## You can get this with the `cardano-options address-hashes` command. It will also
## tell you whether that hash is for a pubkey or a script.
optionsAddrStakingCred="3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa"

contractId="3c4d40a176b599ecb82d45a80795bb8219e1dcd4d84e0dde64f46ff872f7d52f"

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

## Create the minting redeemer. Make sure to properly specify whether the credential is
## a pubkey or a script.
echo "Creating the minting redeemer..."
cardano-options beacon-redeemer mint-active \
  --contract-id $contractId \
  --staking-pubkey-hash $optionsAddrStakingCred \
  --out-file $beaconRedeemerFile

## Helper beacon variables
assetsBeacon="${beaconPolicyId}.${assetsTokenName}"
proposedBeacon="${beaconPolicyId}.${proposedTokenName}"
activeBeacon="${beaconPolicyId}.${activeTokenName}"
contractIDBeacon="${beaconPolicyId}.${contractId}"

## Create the datum for the accepted contract.
echo "Creating the datum..."
cardano-options options-datum accept-datum \
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
  --expiration 27974054 \
  --contract-id $contractId \
  --out-file $acceptDatumFile

## Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-options options-redeemer \
  --purchase-contract \
  --out-file $spendingRedeemerFile

## Create the writer's bech32 address.
echo "Creating the writer's bech32 address..."
writerAddr=$(cardano-options generate-bech32-address \
  --payment-pubkey-hash "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f" \
  --stdout)

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in f2773cfc1529eefa956088c92284590bb5914eff6ac66986ba138e064ddaa169#0 \
  --tx-in 3c4d40a176b599ecb82d45a80795bb8219e1dcd4d84e0dde64f46ff872f7d52f#0 \
  --tx-in-script-file $optionsScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $spendingRedeemerFile \
  --tx-in 96024fb9e9f3aa5112b2eb985aa71658225e3e3e9a4f66509e4a062ec6e481e8#0 \
  --tx-in-script-file $optionsScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $spendingRedeemerFile \
  --tx-out "${optionsAddr} + 15000000 lovelace + 1 ${activeBeacon} + 1 ${contractIDBeacon}" \
  --tx-out-inline-datum-file $acceptDatumFile \
  --tx-out "${writerAddr} + 5000000 lovelace" \
  --tx-out "$(cat ../assets/wallets/02.addr) + 2000000 lovelace + 1 ${contractIDBeacon}" \
  --mint "-1 ${assetsBeacon} + -1 ${proposedBeacon} + 1 ${activeBeacon} + 2 ${contractIDBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address "$(cat ../assets/wallets/02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
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