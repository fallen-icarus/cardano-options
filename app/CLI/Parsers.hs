{-# LANGUAGE OverloadedStrings #-}

module CLI.Parsers
(
  parseCommand
) where

import Options.Applicative
import Data.Text (pack)

import CardanoOptions
import CLI.Types

-------------------------------------------------
-- Main Parsers
-------------------------------------------------
parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "export-script"
      (info parseExportScript $ progDesc "Export a dApp plutus script.")
  , command "options-datum"
      (info parseCreateOptionsDatum $ progDesc "Create a datum for the options validator.")
  , command "options-redeemer"
      (info pCreateOptionsRedeemer $ progDesc "Create a redeemer for the options validator.")
  , command "beacon-redeemer"
      (info parseCreateBeaconRedeemer $ progDesc "Create a redeemer for the beacon policy.")
  , command "convert-address"
      (info pConvertAddress $ progDesc "Convert plutus address <--> Bech32 address.")
  , command "convert-time"
      (info pConvertTime $ progDesc "Convert POSIXTime <--> Slot.")
  , command "query"
      (info parseQueryBeacons $ progDesc "Query the dApp's beacons.")
  ]

-------------------------------------------------
-- Scripts Parser
-------------------------------------------------
parseExportScript :: Parser Command
parseExportScript = hsubparser $ mconcat
    [ command "beacon-policy"
        (info pExportPolicy $ progDesc "Export the beacon policy for a specific trading pair.")
    , command "options-script"
        (info pExportOptions $ progDesc "Export the options validator script.")
    ]
  where
    pExportPolicy :: Parser Command
    pExportPolicy = ExportScript <$> pPolicy <*> pOutputFile

    pExportOptions :: Parser Command
    pExportOptions = ExportScript <$> pure OptionsScript <*> pOutputFile

    pPolicy :: Parser Script
    pPolicy = BeaconPolicy <$> pOptionsConfig

-------------------------------------------------
-- CreateOptionsDatum Parser
-------------------------------------------------
parseCreateOptionsDatum :: Parser Command
parseCreateOptionsDatum = hsubparser $ mconcat
    [ command "assets-datum"
        (info pAssets $ progDesc "Create the datum for creating an Assets UTxO to back a contract.")
    , command "proposal-datum"
        (info pProposal $ progDesc "Create the datum for proposing a contract.")
    , command "active-datum"
        (info pAccept $ progDesc "Create the datum for accepting/updating a contract.")
    ]
  where
    pAssets :: Parser Command
    pAssets = CreateOptionsDatum <$> pAssetsForContract <*> pOutputFile

    pProposal :: Parser Command
    pProposal = CreateOptionsDatum <$> pProposedContract <*> pOutputFile

    pAccept :: Parser Command
    pAccept = CreateOptionsDatum <$> pActiveContract <*> pOutputFile

-------------------------------------------------
-- CreateOptionsRedeemer Parsers
-------------------------------------------------
pCreateOptionsRedeemer :: Parser Command
pCreateOptionsRedeemer = hsubparser $ mconcat
    [ command "close-assets"
        (info pCloseAssets $ progDesc "Close an Assets UTxO.")
    , command "close-proposal"
        (info pCloseProposed $ progDesc "Close Proposal UTxO(s).")
    , command "purchase-contract"
        (info pAcceptContract $ progDesc "Purchase an options contract.")
    , command "execute-contract"
        (info pExecuteContract $ progDesc "Execute an options contract.")
    , command "close-expired-contract"
        (info pCloseExpired $ progDesc "Close and expired contract UTxO.")
    , command "update-address"
        (info pUpdateAddress $ progDesc "Update an ActiveDatum's address")
    ]
  where
    pCloseAssets :: Parser Command
    pCloseAssets = CreateOptionsRedeemer CloseAssets <$> pOutputFile

    pCloseProposed :: Parser Command
    pCloseProposed = CreateOptionsRedeemer CloseProposedContracts <$> pOutputFile

    pAcceptContract :: Parser Command
    pAcceptContract = CreateOptionsRedeemer AcceptContract <$> pOutputFile

    pExecuteContract :: Parser Command
    pExecuteContract = CreateOptionsRedeemer ExecuteContract <$> pOutputFile

    pCloseExpired :: Parser Command
    pCloseExpired = CreateOptionsRedeemer CloseExpiredContract <$> pOutputFile

    pUpdateAddress :: Parser Command
    pUpdateAddress = 
      CreateOptionsRedeemer 
        <$> (UpdateAddress <$> pAddress)
        <*> pOutputFile

-------------------------------------------------
-- CreateBeaconRedeemer Parser
-------------------------------------------------
parseCreateBeaconRedeemer :: Parser Command
parseCreateBeaconRedeemer = hsubparser $ mconcat
    [ command "mint-assets"
        (info pMintAssets $ progDesc "Create the redeemer for minting an Assets beacon.")
    , command "mint-proposal"
        (info pMintProposed $ progDesc "Create the redeemer for minting Proposed beacons.")
    , command "mint-active"
        (info pMintActive $ progDesc "Create the redeemer for minting an Active beacon and ContractID.")
    , command "burn-beacons"
        (info pBurnBeacons $ progDesc "Create the redeemer for burning beacons.")
    ]
  where
    pMintAssets :: Parser Command
    pMintAssets = CreateBeaconRedeemer MintAssetsBeacon <$> pOutputFile

    pMintProposed :: Parser Command
    pMintProposed = 
      CreateBeaconRedeemer MintProposedBeacons <$> pOutputFile

    pMintActive :: Parser Command
    pMintActive = 
      CreateBeaconRedeemer 
        <$> (MintActiveBeacon <$> pContractId <*> pCredential)
        <*> pOutputFile
    
    pBurnBeacons :: Parser Command
    pBurnBeacons = CreateBeaconRedeemer BurnBeacons <$> pOutputFile

    pScriptCredential :: Parser Credential
    pScriptCredential = ScriptCredential <$> option (eitherReader readValidatorHash)
      (  long "staking-script-hash"
      <> metavar "STRING"
      <> help "The hash of the staking script used in the options' address."
      )

    pPubKeyCredential :: Parser Credential
    pPubKeyCredential = PubKeyCredential <$> option (eitherReader readPubKeyHash)
      ( long "staking-pubkey-hash"
      <> metavar "STRING"
      <> help "The hash of the staking pubkey used in the options' address."
      )

    pCredential :: Parser Credential
    pCredential = pPubKeyCredential <|> pScriptCredential

-------------------------------------------------
-- ConvertTime Parser
-------------------------------------------------
pConvertTime :: Parser Command
pConvertTime = ConvertTime <$> (pPOSIXTime <|> pSlot)
  where
    pPOSIXTime :: Parser ConvertTime
    pPOSIXTime = POSIXTimeToSlot . POSIXTime <$> option auto
      (  long "posix-time"
      <> metavar "INT"
      <> help "Convert POSIX time to slot number."
      )

    pSlot :: Parser ConvertTime
    pSlot = SlotToPOSIXTime . Slot <$> option auto
      (  long "slot"
      <> metavar "INT"
      <> help "Convert slot number to POSIX time."
      )

-------------------------------------------------
-- ConvertAddress Parser
-------------------------------------------------
pConvertAddress :: Parser Command
pConvertAddress = 
    ConvertAddress <$> (pBech <|> pPlutus) <*> pOutput
  where
    pBech :: Parser ConvertAddress
    pBech = Bech32 . pack <$> pBech32Address

    pPlutus :: Parser ConvertAddress
    pPlutus = Plutus <$> pAddress

-------------------------------------------------
-- QueryBeacons Parser
-------------------------------------------------
parseQueryBeacons :: Parser Command
parseQueryBeacons = fmap QueryBeacons . hsubparser $ mconcat
    [ command "available-contracts"
        (info pAvailableContracts $ progDesc "Query all available contracts for purchase.")
    , command "own-assets-utxos"
        (info pOwnAssets $ progDesc "Query all own assets UTxOs for a given trading pair.")
    , command "own-proposal-utxos"
        (info pOwnProposals $ progDesc "Query all own proposal UTxOs for a given trading pair.")
    , command "own-active-utxos"
        (info pOwnActive $ progDesc "Query all own active UTxOs for a given trading pair.")
    , command "specific-contract"
        (info pSpecificContract $ progDesc "Query the information for a specific active contract.")
    , command "own-contracts"
        (info pOwnContracts $ progDesc "Lookup all contractIDs in a user's address.")
    ]
  where
    pAvailableContracts :: Parser Query
    pAvailableContracts = QueryAvailableContracts <$> pNetwork <*> pBeaconPolicy <*> pOutput

    pOwnAssets :: Parser Query
    pOwnAssets = QueryOwnAssetsUTxOs <$> pNetwork <*> pBeaconPolicy <*> pOptionsAddr <*> pOutput

    pOwnProposals :: Parser Query
    pOwnProposals = QueryOwnProposedUTxOs <$> pNetwork <*> pBeaconPolicy <*> pOptionsAddr <*> pOutput

    pOwnActive :: Parser Query
    pOwnActive = QueryOwnActiveUTxOs <$> pNetwork <*> pBeaconPolicy <*> pOptionsAddr <*> pOutput

    pSpecificContract :: Parser Query
    pSpecificContract = QuerySpecificContract <$> pNetwork <*> pBeaconPolicy <*> pContractId <*> pOutput

    pOwnContracts :: Parser Query
    pOwnContracts = QueryOwnContracts <$> pNetwork <*> pBeaconPolicy <*> pOptionsAddr <*> pOutput

    pOptionsAddr :: Parser OptionsAddress
    pOptionsAddr = OptionsAddress <$> pBech32Address

-------------------------------------------------
-- Basic Helper Parsers
-------------------------------------------------
pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "The output file."
  <> completer (bashCompleter "file")
  )

pCurrentAsset :: Parser (CurrencySymbol,TokenName)
pCurrentAsset = pCurrentAssetLovelace <|> ((,) <$> pCurrentAssetCurrencySymbol <*> pCurrentAssetTokenName)
  where
    pCurrentAssetLovelace :: Parser (CurrencySymbol,TokenName)
    pCurrentAssetLovelace = flag' (adaSymbol,adaToken)
      (  long "current-asset-is-lovelace"
      <> help "The current asset is lovelace"
      )

    pCurrentAssetCurrencySymbol :: Parser CurrencySymbol
    pCurrentAssetCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "current-asset-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the current asset."
      )

    pCurrentAssetTokenName :: Parser TokenName
    pCurrentAssetTokenName = option (eitherReader readTokenName)
      (  long "current-asset-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the current asset."
      )

pDesiredAsset :: Parser (CurrencySymbol,TokenName)
pDesiredAsset = pDesiredAssetLovelace <|> ((,) <$> pDesiredAssetCurrencySymbol <*> pDesiredAssetTokenName)
  where
    pDesiredAssetLovelace :: Parser (CurrencySymbol,TokenName)
    pDesiredAssetLovelace = flag' (adaSymbol,adaToken)
      (  long "desired-asset-is-lovelace"
      <> help "The desired asset is lovelace"
      )

    pDesiredAssetCurrencySymbol :: Parser CurrencySymbol
    pDesiredAssetCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "desired-asset-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the desired asset."
      )

    pDesiredAssetTokenName :: Parser TokenName
    pDesiredAssetTokenName = option (eitherReader readTokenName)
      (  long "desired-asset-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the desired asset."
      )

pOptionsConfig :: Parser OptionsConfig
pOptionsConfig = OptionsConfig <$> pCurrentAsset <*> pDesiredAsset

pBeaconPolicy :: Parser CurrencySymbol
pBeaconPolicy = option (eitherReader readCurrencySymbol)
  (  long "beacon-policy-id"
  <> metavar "STRING"
  <> help "Policy id for that trading pair's beacon policy.")

pCurrentAssetQuantity :: Parser Integer
pCurrentAssetQuantity = option auto
  (  long "quantity"
  <> metavar "INT"
  <> help "Quantity of current asset to be traded."
  )

pAssetsForContract :: Parser OptionsDatum
pAssetsForContract = 
  AssetsForContract 
    <$> pBeaconPolicy 
    <*> pCurrentAsset 
    <*> pCurrentAssetQuantity
    <*> pDesiredAsset

pStrikePrice :: Parser PlutusRational
pStrikePrice = unsafeRatio <$> pStrikePriceNum <*> pStrikePriceDen
  where
    pStrikePriceNum :: Parser Integer
    pStrikePriceNum = option auto
      ( long "strike-price-numerator"
      <> metavar "INT"
      <> help "The numerator of the strike price."
      )

    pStrikePriceDen :: Parser Integer
    pStrikePriceDen = option auto
      ( long "strike-price-denominator"
      <> metavar "INT"
      <> help "The denominator of the strike price."
      )

pAddress :: Parser Address
pAddress = 
    Address
      <$> pPaymentCredential
      <*> (pStakingCredential <|> pure Nothing)
  where
    pPaymentScriptCredential :: Parser Credential
    pPaymentScriptCredential = ScriptCredential <$> option (eitherReader readValidatorHash)
      (  long "payment-script-hash"
      <> metavar "STRING"
      <> help "The hash of the payment script used in the address."
      )

    pPaymentPubKeyCredential :: Parser Credential
    pPaymentPubKeyCredential = PubKeyCredential <$> option (eitherReader readPubKeyHash)
      ( long "payment-pubkey-hash"
      <> metavar "STRING"
      <> help "The hash of the payment pubkey used in the address."
      )

    pPaymentCredential :: Parser Credential
    pPaymentCredential = pPaymentPubKeyCredential <|> pPaymentScriptCredential

    pStakingScriptCredential :: Parser StakingCredential
    pStakingScriptCredential = StakingHash . ScriptCredential <$> option (eitherReader readValidatorHash)
      (  long "staking-script-hash"
      <> metavar "STRING"
      <> help "The hash of the staking script used in the address."
      )

    pStakingPubKeyCredential :: Parser StakingCredential
    pStakingPubKeyCredential = StakingHash . PubKeyCredential <$> option (eitherReader readPubKeyHash)
      (  long "staking-pubkey-hash"
      <> metavar "STRING"
      <> help "The hash of the staking pubkey used in the address."
      )

    pStakingCredential :: Parser (Maybe StakingCredential)
    pStakingCredential = Just <$> (pStakingPubKeyCredential <|> pStakingScriptCredential)

pPremiumAsset :: Parser (CurrencySymbol,TokenName)
pPremiumAsset = pPremiumAssetLovelace <|> ((,) <$> pPremiumAssetCurrencySymbol <*> pPremiumAssetTokenName)
  where
    pPremiumAssetLovelace :: Parser (CurrencySymbol,TokenName)
    pPremiumAssetLovelace = flag' (adaSymbol,adaToken)
      (  long "premium-asset-is-lovelace"
      <> help "The premium asset is lovelace"
      )

    pPremiumAssetCurrencySymbol :: Parser CurrencySymbol
    pPremiumAssetCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "premium-asset-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the premium asset."
      )

    pPremiumAssetTokenName :: Parser TokenName
    pPremiumAssetTokenName = option (eitherReader readTokenName)
      (  long "premium-asset-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the premium asset."
      )

pPremium :: Parser Integer
pPremium = option auto
  (  long "premium"
  <> metavar "INT"
  <> help "The amount for the premium."
  )

pExpiration :: Parser POSIXTime
pExpiration = slotToPOSIXTime . Slot <$> option auto
  (  long "expiration"
  <> metavar "INT"
  <> help "The slot at which the contract expires."
  )

pProposedContract :: Parser OptionsDatum
pProposedContract = 
  ProposedContract
    <$> pBeaconPolicy
    <*> pCurrentAsset
    <*> pCurrentAssetQuantity
    <*> pDesiredAsset
    <*> pStrikePrice
    <*> pAddress -- ^ Creator's address
    <*> pPremiumAsset
    <*> pPremium
    <*> pExpiration

pContractId :: Parser TokenName
pContractId = option (eitherReader readTokenName)
  (  long "contract-id"
  <> metavar "STRING"
  <> help "The ContractID for this contract."
  )

pActiveContract :: Parser OptionsDatum
pActiveContract = 
  ActiveContract
    <$> pBeaconPolicy
    <*> pCurrentAsset
    <*> pCurrentAssetQuantity
    <*> pDesiredAsset
    <*> pStrikePrice
    <*> pAddress -- ^ Creator's address
    <*> pPremiumAsset
    <*> pPremium
    <*> pExpiration
    <*> pContractId

pBech32Address :: Parser String
pBech32Address = strOption
  (  long "address"
  <> metavar "STRING"
  <> help "Address in bech32 format."
  )

pOutput :: Parser Output
pOutput = pStdOut <|> File <$> pOutputFile
  where
    pStdOut :: Parser Output
    pStdOut = flag' Stdout
      (  long "stdout"
      <> help "Display to stdout."
      )

pNetwork :: Parser Network
pNetwork = pPreProdTestnet
  where
    pPreProdTestnet :: Parser Network
    pPreProdTestnet = PreProdTestnet <$> strOption
      (  long "preprod-testnet"
      <> metavar "STRING"
      <> help "Query the preproduction testnet using the Blockfrost Api with the supplied api key.")