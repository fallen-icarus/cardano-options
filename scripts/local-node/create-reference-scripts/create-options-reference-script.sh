#!/bin/sh

# A helper script for showing how to store the scripts on chain as reference scripts.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
optionsDir="${mainDir}options-files/"
tmpDir="${mainDir}tmp/"

scriptFile="${optionsDir}options.plutus"

## Export the script.
echo "Exporting the options script..."
cardano-options scripts \
  --options-script \
  --out-file $scriptFile

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in 9c23472cb2e7787861618c91f7a7a28df71d04bc69176c4c79e656ccc8ccedb1#0 \
  --tx-in 6ff90f518608f6bb6ce88e55ec8b9da1f108efa6a407e6899b0f5198ed2f1f3d#1 \
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
