#!/bin/sh

# A helper script for showing how to store the scripts on chain as reference scripts.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
optionsDir="${mainDir}options-files/"
tmpDir="${mainDir}tmp/"

scriptFile="${optionsDir}address_update_observer.plutus"

## Export the script.
echo "Exporting the address update observer script..."
cardano-options scripts \
  --address-update-script \
  --out-file $scriptFile

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in a7c0a2283eb804111bd39f40f9e9cafa05d6da14d4bf8d20d2aaa67b5fb0cde6#1 \
  --tx-in a7c0a2283eb804111bd39f40f9e9cafa05d6da14d4bf8d20d2aaa67b5fb0cde6#0 \
  --tx-out "$(cat "${walletDir}01.addr") + 15000000 lovelace" \
  --tx-out-reference-script-file $scriptFile \
  --change-address "$(cat "${walletDir}01.addr")" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
