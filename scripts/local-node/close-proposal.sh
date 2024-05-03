#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
optionsDir="${mainDir}options-files/"
tmpDir="${mainDir}tmp/"

writerStakePubKeyFile="${walletDir}01Stake.vkey"

beaconRedeemerFile="${optionsDir}createCloseOrUpdateProposal.json"
optionsRedeemerFile="${optionsDir}closeOrUpdateProposal.json"

premiumAsset='lovelace'
offerAsset='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
askAsset='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the writer..."
writerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $writerStakePubKeyFile)

## Create the required redeemers.
echo "Creating the proposal beacon redeemer..."
cardano-options redeemers proposal-script manage-proposals \
  --out-file $beaconRedeemerFile

echo "Creating the spending redeemer..."
cardano-options redeemers options-script manage-proposal \
  --out-file $optionsRedeemerFile

## Get the proposal beacon policy id.
echo "Calculating the proposal beacon policy id..."
beaconPolicyId=$(cardano-options beacon-name policy-id \
  --proposal-beacons \
  --stdout) 

## Get the required beacon names.
offerBeaconName=$(cardano-options beacon-name asset-name offer-beacon \
  --offer-asset $offerAsset \
  --stdout)
askBeaconName=$(cardano-options beacon-name asset-name ask-beacon \
  --ask-asset $askAsset \
  --stdout)
premiumBeaconName=$(cardano-options beacon-name asset-name premium-beacon \
  --premium-asset $premiumAsset \
  --stdout)
pairBeaconName=$(cardano-options beacon-name asset-name trading-pair-beacon \
  --offer-asset $offerAsset \
  --ask-asset $askAsset \
  --stdout)

offerBeacon="${beaconPolicyId}.${offerBeaconName}"
askBeacon="${beaconPolicyId}.${askBeaconName}"
premiumBeacon="${beaconPolicyId}.${premiumBeaconName}"
pairBeacon="${beaconPolicyId}.${pairBeaconName}"

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in 79a30b0dd58d8bda1d6f2202dfc5112e4d981dbdf6ad24294f1cd7da03ba6332#2 \
  --spending-tx-in-reference 9c23472cb2e7787861618c91f7a7a28df71d04bc69176c4c79e656ccc8ccedb1#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $optionsRedeemerFile \
  --tx-out "$(cat ${walletDir}01.addr) + 2000000 lovelace + 10 ${offerAsset}" \
  --mint "-1 ${askBeacon} + -1 ${offerBeacon} + -1 ${pairBeacon} + -1 ${premiumBeacon}" \
  --mint-tx-in-reference a1797d0186118d658ca66156c4ecd669073ec79282e65cbbda19d2ada41ebe71#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --required-signer-hash $writerStakePubKeyHash \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --signing-key-file "${walletDir}/01Stake.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
