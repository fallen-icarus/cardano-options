#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
optionsDir="${mainDir}options-files/"
tmpDir="${mainDir}tmp/"

activeRedeemerFile="${optionsDir}burnAll.json"

contractIdName="127f33c969bff4e22750572ae2d512d33013954b01f471e291443931823c66b1"

## Create the required redeemers.
echo "Creating the active redeemer..."
cardano-options redeemers active-script burn-all \
  --out-file $activeRedeemerFile

## Get the active beacon policy id.
echo "Calculating the active beacon policy id..."
activePolicyId=$(cardano-options beacon-name policy-id \
  --active-beacons \
  --stdout) 

extraKey="${activePolicyId}.${contractIdName}"

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in b806ae2059c34764803a8bb0c238b1b81a7817c55dad22145078e4832524d8df#2 \
  --mint "-1 ${extraKey}" \
  --mint-tx-in-reference ef3a86147093fe25f4b056e82f52439f4ca71ac898ec0074fd113fb5054a4b45#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeRedeemerFile \
  --policy-id $activePolicyId \
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
