#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
optionsDir="${mainDir}options-files/"
tmpDir="${mainDir}tmp/"

optionsScript="${optionsDir}options.plutus"

writerStakePubKeyFile="${walletDir}01Stake.vkey"

proposalDatumFile="${optionsDir}proposalDatum.json"
beaconRedeemerFile="${optionsDir}createCloseOrUpdateProposal.json"

premiumAsset='lovelace'
offerAsset='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
askAsset='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

contractDeposit=5000000 # 5 ADA

currentSlot=$(cardano-options query current-slot --testnet)

## The first expiration will be 2000 slots from now.
expirationSlot1=$((currentSlot + 2000))
expirationTime1=$(cardano-options convert-time --slot $expirationSlot1 --testnet)

## The second expiration will be 3000 slots from now.
expirationSlot2=$((currentSlot + 3000))
expirationTime2=$(cardano-options convert-time --slot $expirationSlot2 --testnet)

## Export the scripts.
echo "Exporting the scripts..."
cardano-options scripts \
  --options-script \
  --out-file $optionsScript

## Create the options address.
echo "Creating the writer's options address..."
writerOptionsAddr=$(cardano-cli address build \
  --payment-script-file $optionsScript \
  --stake-verification-key-file $writerStakePubKeyFile \
  --testnet-magic 1)

## Create the proposal datum.
## Make sure premium, strike-price, and expiration are paired up!
echo "Creating the proposal datum..."
cardano-options datums proposal \
  --offer-asset $offerAsset \
  --quantity 10 \
  --ask-asset $askAsset \
  --premium-asset $premiumAsset \
  --deposit $contractDeposit \
  --payment-address "$(cat ${walletDir}01.addr)" \
  --premium 2000000 \
  --strike-price "1 / 1" \
  --expiration $expirationTime1 \
  --premium 3000000 \
  --strike-price "1 / 2" \
  --expiration $expirationTime2 \
  --out-file $proposalDatumFile

## Create the required redeemer.
echo "Creating the proposal beacon redeemer..."
cardano-options redeemers proposal-script manage-proposals \
  --out-file $beaconRedeemerFile

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
## invalid-hereafter should be set to the earliest expiration slot.
cardano-cli transaction build \
  --tx-in 1e8ba02a6e6fb5a151777c6efe8139b1a0f823bb70ec46efdebeb3778a129869#1 \
  --tx-in 08da3a6ff7dfe5b20d50d3773406f6b95f6f9c4339456219d826d4be3274f7b1#0 \
  --tx-out "${writerOptionsAddr} + ${contractDeposit} lovelace + 1 ${askBeacon} + 1 ${offerBeacon} + 1 ${premiumBeacon} + 1 ${pairBeacon} + 10 ${offerAsset}" \
  --tx-out-inline-datum-file $proposalDatumFile \
  --mint "1 ${askBeacon} + 1 ${offerBeacon} + 1 ${pairBeacon} + 1 ${premiumBeacon}" \
  --mint-tx-in-reference 0f070c06650f0aec11352496afc0ff684398d2eb60fd73ab262079119930f030#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --invalid-hereafter $expirationSlot1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
