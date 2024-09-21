#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
optionsDir="${mainDir}options-files/"
tmpDir="${mainDir}tmp/"

addressUpdateObserverScript="${optionsDir}addressObserver.plutus"

observerRedeemerFile="${optionsDir}observerAddressUpdate.json"
optionsRedeemerFile="${optionsDir}updateAddress.json"

writerStakePubKeyFile="${walletDir}01Stake.vkey"
writerAddr="addr_test1zprnu4c2twkgwep43eytwlhlfmc8ezv85q3cvnrslnxfsafualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aque7h0y"

newPaymentAddr="$(cat ${walletDir}01.addr)"

activeDatumFile="${optionsDir}activeDatum.json"

offerAsset='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
askAsset='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

depositIncrease=1000000 # 1 ADA
contractDeposit=5000000 # 5 ADA
newDeposit=$((contractDeposit + depositIncrease))
contractUTxO="5d6f286fb8f2fc173e9196d29ca92baa26a79a8301590789a36c5141687c22aa#1"
contractIdName="522c821f25a8feca8cfcfc9711c9cf2eb9ef470a0efaf4bc49c89c53bdb5b576"

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the writer..."
writerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $writerStakePubKeyFile)

## Export the address update observer script so you can generate its required stake address.
echo "Exporting the address update observer script..."
cardano-options scripts \
  --address-update-script \
  --out-file $addressUpdateObserverScript

## Build the observer script's stake address.
echo "Building the observer script's stake address..."
observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file $addressUpdateObserverScript)

## Create the required redeemers.
echo "Creating the observer redeemer..."
cardano-options redeemers address-update-script observe-address-update \
  --out-file $observerRedeemerFile

echo "Creating the options spending redeemer..."
cardano-options redeemers options-script update-payment-address \
  --payment-address "$newPaymentAddr" \
  --deposit-increase $depositIncrease \
  --out-file $optionsRedeemerFile

## Create the Active datum.
echo "Creating the active datum..."

cardano-options datums active post-address-update auto \
  --testnet \
  --contract-ref $contractUTxO \
  --payment-address "$newPaymentAddr"\
  --deposit-increase $depositIncrease \
  --out-file $activeDatumFile

# cardano-options datums active post-address-update manual \
#   --offer-asset $offerAsset \
#   --quantity 10 \
#   --ask-asset $askAsset \
#   --deposit $contractDeposit \
#   --payment-address "$newPaymentAddr" \
#   --strike-price "1 / 1" \
#   --expiration 1714744278000  \
#   --contract-id $contractIdName \
#   --out-file $activeDatumFile

## Get the active beacon policy id.
echo "Calculating the active beacon policy id..."
activeBeaconPolicyId=$(cardano-options beacon-name policy-id \
  --active-beacons \
  --stdout) 

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

# Create and submit the transaction.
cardano-cli transaction build \
  --tx-in afdd5ccb5d00f4f2162d37768c45ac4450721b59d6ca5ed665725b40a455521e#1 \
  --tx-in $contractUTxO \
  --spending-tx-in-reference afdd5ccb5d00f4f2162d37768c45ac4450721b59d6ca5ed665725b40a455521e#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $optionsRedeemerFile \
  --tx-out "${writerAddr} + ${newDeposit} lovelace + 1 ${activeOfferBeacon} + 1 ${activeAskBeacon} + 1 ${activePairBeacon} + 1 ${activeContractId} + 10 ${offerAsset}" \
  --tx-out-inline-datum-file $activeDatumFile \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference 27ef87944b571eecd9618461f496245f00e229f2e5004f1798b097312b1803ad#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
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
