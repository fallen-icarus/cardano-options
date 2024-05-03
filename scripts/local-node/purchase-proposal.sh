#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
optionsDir="${mainDir}options-files/"
tmpDir="${mainDir}tmp/"

writerAddr="addr_test1zprnu4c2twkgwep43eytwlhlfmc8ezv85q3cvnrslnxfsafualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aque7h0y"
paymentAddress="addr_test1vrlfp27zjnjlsak5f7dnjkpl9ekeq5ezc3e4uw769y5rgtc4qvv2f"

activeDatumFile="${optionsDir}activeDatum.json"
paymentDatumFile="${optionsDir}paymentDatum.json"
proposalBeaconRedeemerFile="${optionsDir}burnProposalBeacons.json"
activeBeaconRedeemerFile="${optionsDir}purchaseExecuteOrCloseExpired.json"
optionsRedeemerFile="${optionsDir}purchaseContract.json"

premiumAsset='lovelace'
offerAsset='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
askAsset='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

contractDeposit=5000000 # 5 ADA
desiredTermsIndex=0 # The first terms in the possilbe_terms list.
premium=2000000
strikePrice="1 / 1"
expiration=1714755185000 # in posix
proposalUTxO="7a1d3bb453b1ea065f391529d1a56e7e02bc03f4dc802acf3a1967dc447c3259#0"

## Create the Active datum.
echo "Creating the active datum..."

cardano-options datums active new auto \
  --testnet \
  --proposal-ref $proposalUTxO \
  --desired-terms-index $desiredTermsIndex \
  --out-file $activeDatumFile

# cardano-options datums active new manual \
#   --offer-asset $offerAsset \
#   --quantity 10 \
#   --ask-asset $askAsset \
#   --deposit $contractDeposit \
#   --payment-address "$(cat ${walletDir}01.addr)" \
#   --strike-price "$strikePrice" \
#   --expiration $expiration \
#   --proposal-ref $proposalUTxO \
#   --out-file $activeDatumFile

## Create the required redeemers.
echo "Creating the proposal beacon redeemer..."
cardano-options redeemers proposal-script burn-all \
  --out-file $proposalBeaconRedeemerFile

echo "Creating the active beacon redeemer..."
cardano-options redeemers active-script main \
  --out-file $activeBeaconRedeemerFile

echo "Creating the options spending redeemer..."
cardano-options redeemers options-script purchase \
  --desired-terms-index $desiredTermsIndex \
  --out-file $optionsRedeemerFile

## Get the proposal beacon policy id.
echo "Calculating the proposal beacon policy id..."
proposalBeaconPolicyId=$(cardano-options beacon-name policy-id \
  --proposal-beacons \
  --stdout) 

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
premiumBeaconName=$(cardano-options beacon-name asset-name premium-beacon \
  --premium-asset $premiumAsset \
  --stdout)
pairBeaconName=$(cardano-options beacon-name asset-name trading-pair-beacon \
  --offer-asset $offerAsset \
  --ask-asset $askAsset \
  --stdout)
contractIdName=$(cardano-options beacon-name asset-name contract-id \
  --proposal-ref $proposalUTxO \
  --stdout)

proposalOfferBeacon="${proposalBeaconPolicyId}.${offerBeaconName}"
proposalAskBeacon="${proposalBeaconPolicyId}.${askBeaconName}"
proposalPremiumBeacon="${proposalBeaconPolicyId}.${premiumBeaconName}"
proposalPairBeacon="${proposalBeaconPolicyId}.${pairBeaconName}"

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
  --tx-in 5c900c78e13e67355b99532d4cc094a231745fab66c60201a3228143a3a2e0a9#0 \
  --tx-in $proposalUTxO \
  --spending-tx-in-reference afdd5ccb5d00f4f2162d37768c45ac4450721b59d6ca5ed665725b40a455521e#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $optionsRedeemerFile \
  --tx-out "${paymentAddress} + ${premium} lovelace" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --tx-out "${writerAddr} + ${contractDeposit} lovelace + 1 ${activeOfferBeacon} + 1 ${activeAskBeacon} + 1 ${activePairBeacon} + 1 ${activeContractId} + 10 ${offerAsset}" \
  --tx-out-inline-datum-file $activeDatumFile \
  --mint "-1 ${proposalAskBeacon} + -1 ${proposalOfferBeacon} + -1 ${proposalPremiumBeacon} + -1 ${proposalPairBeacon} + 1 ${activeOfferBeacon} + 1 ${activeAskBeacon} + 1 ${activePairBeacon} + 2 ${activeContractId}" \
  --mint-tx-in-reference 0f070c06650f0aec11352496afc0ff684398d2eb60fd73ab262079119930f030#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $proposalBeaconRedeemerFile \
  --policy-id $proposalBeaconPolicyId \
  --mint-tx-in-reference ef3a86147093fe25f4b056e82f52439f4ca71ac898ec0074fd113fb5054a4b45#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeBeaconRedeemerFile \
  --policy-id $activeBeaconPolicyId \
  --change-address "$(cat ${walletDir}02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/02.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
