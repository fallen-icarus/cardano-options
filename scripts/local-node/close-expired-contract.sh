#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
optionsDir="${mainDir}options-files/"
tmpDir="${mainDir}tmp/"

activeBeaconRedeemerFile="${optionsDir}purchaseExecuteOrCloseExpired.json"
optionsRedeemerFile="${optionsDir}closeExpired.json"

writerStakePubKeyFile="${walletDir}01Stake.vkey"

offerAsset='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
askAsset='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

expiration=1714744278000 # in posix
contractUTxO="1e8ba02a6e6fb5a151777c6efe8139b1a0f823bb70ec46efdebeb3778a129869#0"
contractIdName="127f33c969bff4e22750572ae2d512d33013954b01f471e291443931823c66b1"

## Convert the expiration to the associated slot number for use as invalid-before.
expirationSlot=$(cardano-options convert-time --posix-time $expiration --testnet)

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the writer..."
writerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $writerStakePubKeyFile)

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
cardano-options redeemers options-script close-expired \
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

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in $contractUTxO \
  --spending-tx-in-reference afdd5ccb5d00f4f2162d37768c45ac4450721b59d6ca5ed665725b40a455521e#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $optionsRedeemerFile \
  --mint "-1 ${activeOfferBeacon} + -1 ${activeAskBeacon} + -1 ${activePairBeacon} + -1 ${activeContractId}" \
  --mint-tx-in-reference ef3a86147093fe25f4b056e82f52439f4ca71ac898ec0074fd113fb5054a4b45#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeBeaconRedeemerFile \
  --policy-id $activeBeaconPolicyId \
  --required-signer-hash $writerStakePubKeyHash \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --invalid-before $expirationSlot \
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
