#!/bin/sh

# A helper script for showing how to register the scripts for staking executions.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
optionsDir="${mainDir}options-files/"
tmpDir="${mainDir}tmp/"

proposalScript="${optionsDir}proposal_beacons.plutus"
addressUpdateScript="${optionsDir}address_update_observer.plutus"

proposalCert="${tmpDir}proposal_beacons.cert"
addressUpdateCert="${tmpDir}address_update_observer.cert"

## Export the scripts.
echo "Exporting the scripts..."
cardano-options scripts \
  --proposal-script \
  --out-file $proposalScript

cardano-options scripts \
  --address-update-script \
  --out-file $addressUpdateScript

## Create the registration certificates.
echo "Creating the registration certificates..."
cardano-cli stake-address registration-certificate \
  --stake-script-file $proposalScript \
  --out-file $proposalCert

cardano-cli stake-address registration-certificate \
  --stake-script-file $addressUpdateScript \
  --out-file $addressUpdateCert

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in 04f467c798753dd43761f4eb7a70a2fa1e07977d198079e9fc364834c535afe3#1 \
  --change-address "$(cat "${walletDir}01.addr")" \
  --certificate-file $proposalCert \
  --certificate-file $addressUpdateCert \
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
