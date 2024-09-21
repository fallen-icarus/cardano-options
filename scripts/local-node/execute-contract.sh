#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
optionsDir="${mainDir}options-files/"
tmpDir="${mainDir}tmp/"

paymentAddress="addr_test1vrlfp27zjnjlsak5f7dnjkpl9ekeq5ezc3e4uw769y5rgtc4qvv2f"

paymentDatumFile="${optionsDir}paymentDatum.json"
activeBeaconRedeemerFile="${optionsDir}purchaseExecuteOrCloseExpired.json"
optionsRedeemerFile="${optionsDir}executeContract.json"

offerAsset='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
askAsset='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

contractDeposit=6000000 # 6 ADA
expiration=1714755185000 # in posix
contractUTxO="0094644ca620e01f8d9594f6b2ce6a4186ffcc09ff6f4d6fe72fbe99b850ae41#0"
contractIdName="522c821f25a8feca8cfcfc9711c9cf2eb9ef470a0efaf4bc49c89c53bdb5b576"

## Convert the expiration to the associated slot number for use as invalid-hereafter.
expirationSlot=$(cardano-options convert-time --posix-time $expiration --testnet)

## Get the active beacon policy id.
echo "Calculating the active beacon policy id..."
activeBeaconPolicyId=$(cardano-options beacon-name policy-id \
  --active-beacons \
  --stdout) 

## Create the required redeemers.
echo "Creating the active beacon redeemer..."
cardano-options redeemers active-script main \
  --out-file $activeBeaconRedeemerFile

echo "Creating the options spending redeemer..."
cardano-options redeemers options-script execute \
  --out-file $optionsRedeemerFile

## Get the required beacon names.
offerBeaconName=$(cardano-options beacon-name asset-name offer-beacon \
  --offer-asset $offerAsset \
  --stdout)
askBeaconName=$(cardano-options beacon-name asset-name ask-beacon \
  --ask-asset $askAsset \
  --stdout)
pairBeaconName=$(cardano-options beacon-name asset-name trading-pair-beacon \
  --offer-asset $offerAsset \
  --ask-asset $askAsset \
  --stdout)

activeOfferBeacon="${activeBeaconPolicyId}.${offerBeaconName}"
activeAskBeacon="${activeBeaconPolicyId}.${askBeaconName}"
activePairBeacon="${activeBeaconPolicyId}.${pairBeaconName}"
activeContractId="${activeBeaconPolicyId}.${contractIdName}"

## Create the payment datum.
echo "Creating the payment datum..."
cardano-options datums payment \
  --contract-id $contractIdName \
  --out-file $paymentDatumFile

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in 5d6f286fb8f2fc173e9196d29ca92baa26a79a8301590789a36c5141687c22aa#2 \
  --tx-in 7e95de24a4ada8ccd315b01d2f08d845d05e370b6f33e2f9238c80f66d2a4582#0 \
  --tx-in $contractUTxO \
  --spending-tx-in-reference afdd5ccb5d00f4f2162d37768c45ac4450721b59d6ca5ed665725b40a455521e#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $optionsRedeemerFile \
  --tx-out "${paymentAddress} + ${contractDeposit} lovelace + 10 ${askAsset}" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --tx-out "$(cat ${walletDir}02.addr) + 3000000 lovelace + 12 ${askAsset}" \
  --mint "-1 ${activeOfferBeacon} + -1 ${activeAskBeacon} + -1 ${activePairBeacon} + -2 ${activeContractId}" \
  --mint-tx-in-reference ef3a86147093fe25f4b056e82f52439f4ca71ac898ec0074fd113fb5054a4b45#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeBeaconRedeemerFile \
  --policy-id $activeBeaconPolicyId \
  --change-address "$(cat ${walletDir}02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --testnet-magic 1 \
  --invalid-hereafter $expirationSlot \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/02.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
