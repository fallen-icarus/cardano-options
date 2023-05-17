#!/bin/sh

# A helper script for showing how to create ProposalContract UTxOs.

## Variables
dir="../assets/options-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

optionsAddrFile="${dir}options.addr"

proposalDatumFile1="${dir}proposal1.json"
proposalDatumFile2="${dir}proposal2.json"
proposalDatumFile3="${dir}proposal3.json"

beaconRedeemerFile="${dir}mintProposal.json"

proposedTokenName="50726f706f736564" # This is the hexidecimal encoding for 'Proposed'.

writerStakingPubKeyFile="../assets/wallets/01Stake.vkey"

slotTip=28650789

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the writer..."
writerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $writerStakingPubKeyFile)

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
cardano-options beacon-redeemer mint-proposal \
  --out-file $beaconRedeemerFile

## Helper Proposed beacon variable
proposedBeacon="${beaconPolicyId}.${proposedTokenName}"

## Create the datum(s).
echo "Creating the datum(s)..."
cardano-options options-datum proposal-datum \
  --beacon-policy-id $beaconPolicyId \
  --current-asset-is-lovelace \
  --quantity 10000000 \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --strike-price-numerator 1 \
  --strike-price-denominator 1000000 \
  --payment-pubkey-hash "$(cat ../assets/wallets/01.pkh)" \
  --premium-asset-is-lovelace \
  --premium 2000000 \
  --expiration $((slotTip + 3600)) \
  --out-file $proposalDatumFile1

cardano-options options-datum proposal-datum \
  --beacon-policy-id $beaconPolicyId \
  --current-asset-is-lovelace \
  --quantity 10000000 \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --strike-price-numerator 1 \
  --strike-price-denominator 1000000 \
  --payment-pubkey-hash "$(cat ../assets/wallets/01.pkh)" \
  --premium-asset-is-lovelace \
  --premium 1500000 \
  --expiration $((slotTip + 600)) \
  --out-file $proposalDatumFile2

cardano-options options-datum proposal-datum \
  --beacon-policy-id $beaconPolicyId \
  --current-asset-is-lovelace \
  --quantity 10000000 \
  --desired-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --desired-asset-token-name 4f74686572546f6b656e0a \
  --strike-price-numerator 1 \
  --strike-price-denominator 1000000 \
  --payment-pubkey-hash "$(cat ../assets/wallets/01.pkh)" \
  --premium-asset-is-lovelace \
  --premium 1500000 \
  --expiration $((slotTip + 1200)) \
  --out-file $proposalDatumFile3

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 4f7a9f56810532dbcb421cb1c9601806dae5bc4a07eea1e6869ed4db7e70a51e#3 \
  --tx-out "$(cat ${optionsAddrFile}) + 3000000 lovelace + 1 ${proposedBeacon}" \
  --tx-out-inline-datum-file $proposalDatumFile1 \
  --tx-out "$(cat ${optionsAddrFile}) + 3000000 lovelace + 1 ${proposedBeacon}" \
  --tx-out-inline-datum-file $proposalDatumFile2 \
  --tx-out "$(cat ${optionsAddrFile}) + 3000000 lovelace + 1 ${proposedBeacon}" \
  --tx-out-inline-datum-file $proposalDatumFile3 \
  --mint "3 ${proposedBeacon}" \
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