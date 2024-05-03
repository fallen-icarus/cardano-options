module CLI.Parsers
(
  parseCommand
) where

import Options.Applicative
import Relude

import CardanoOptions

import CLI.Data.ApiService
import CLI.Data.Bech32Address
import CLI.Data.Commands
import CLI.Data.Network
import CLI.Data.Output

-------------------------------------------------
-- Main Parsers
-------------------------------------------------
parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "scripts" $
      info parseExportScript $ progDesc "Export a protocol plutus script."
  , command "datums" $
      info parseCreateDatum $ progDesc "Create a datum for the protocol."
  , command "redeemers" $
      info parseCreateRedeemer $ progDesc "Create a redeemer for the protocol."
  , command "beacon-name" $
      info parseBeaconName $ progDesc "Calculate a beacon policy id or asset name."
  , command "convert-time" $
      info pConvertTime $ progDesc "Convert POSIXTime <--> Slot."
  , command "query" $
      info parseQuery $ progDesc "Query the blockchain."
  , command "submit" $
      info pSubmitTx $ progDesc "Submit a transaction to the blockchain."
  , command "protocol-params" $
      info pExportParams $ progDesc "Export the current protocol parameters."
  , command "evaluate-tx" $
      info pEvaluateTx $ progDesc "Estimate script execution units for a transaction."
  ]

-------------------------------------------------
-- Scripts Parser
-------------------------------------------------
parseExportScript :: Parser Command
parseExportScript = 
    ExportScript
      <$> pScript
      <*> pOutputFile
  where
    pScript :: Parser Script
    pScript = pProposalScript
          <|> pActiveScript
          <|> pAddressUpdateScript
          <|> pOptionsScript
          <|> pProxyScript

    pProposalScript :: Parser Script
    pProposalScript = flag' ProposalBeaconScript
      (  long "proposal-script"
      <> help "Export the proposal beacon script."
      )

    pActiveScript :: Parser Script
    pActiveScript = flag' ActiveBeaconScript
      (  long "active-script"
      <> help "Export the active beacon script."
      )

    pAddressUpdateScript :: Parser Script
    pAddressUpdateScript = flag' AddressUpdateObserverScript
      (  long "address-update-script"
      <> help "Export the address update observer script."
      )

    pOptionsScript :: Parser Script
    pOptionsScript = flag' OptionsScript
      (  long "options-script"
      <> help "Export the options spending script."
      )

    pProxyScript :: Parser Script
    pProxyScript = flag' ProxyScript
      (  long "proxy-script"
      <> help "Export the proxy script."
      )

-------------------------------------------------
-- CreateDatum Parser
-------------------------------------------------
parseCreateDatum :: Parser Command
parseCreateDatum = hsubparser $ mconcat
  [ command "proposal" $
      info pCreateNewProposalInfo $ progDesc "Create a ProposalDatum."
  , command "active" $
      info pCreateActiveDatum $ progDesc "Create an ActiveDatum."
  , command "payment" $
      info pCreatePaymentDatum $ progDesc "Create a PaymentDatum."
  ]

pCreatePaymentDatum :: Parser Command
pCreatePaymentDatum = 
  CreateDatum 
    <$> (NewPaymentDatum <$> pContractId)
    <*> pOutputFile

pCreateNewProposalInfo :: Parser Command
pCreateNewProposalInfo =
    CreateDatum
      <$> pNewProposalInfo
      <*> pOutputFile
  where
    pNewProposalInfo :: Parser NewDatum
    pNewProposalInfo =
      fmap NewProposalDatum $ NewProposalInfo
        <$> pOfferAsset
        <*> pOfferQuantity
        <*> pAskAsset
        <*> pPremiumAsset
        <*> pContractDeposit
        <*> pPaymentAddress
        <*> some pPossibleTerm

pCreateActiveDatum :: Parser Command
pCreateActiveDatum = hsubparser $ mconcat
  [ command "new" $
      info pCreateNewActiveInfo $ progDesc "Create a new ActiveDatum."
  , command "post-address-update" $
      info pCreatePostAddressUpdateActive $ progDesc "Create a post-address-update ActiveDatum."
  ]

pCreateNewActiveInfo :: Parser Command
pCreateNewActiveInfo = hsubparser $ mconcat
  [ command "manual" $
      info pCreateNewActiveInfoManual $ progDesc "Create a new ActiveDatum manually."
  , command "auto" $
      info pCreateNewActiveInfoAuto $ progDesc "Create a new ActiveDatum by looking up the proposal UTxO."
  ]

pCreateNewActiveInfoManual :: Parser Command
pCreateNewActiveInfoManual =
    CreateDatum
      <$> pNewActiveInfo
      <*> pOutputFile
  where
    pNewActiveInfo :: Parser NewDatum
    pNewActiveInfo =
      fmap NewActiveDatumManual $ NewActiveInfo
        <$> pOfferAsset
        <*> pOfferQuantity
        <*> pAskAsset
        <*> pStrikePrice
        <*> pExpiration
        <*> pContractDeposit
        <*> pPaymentAddress
        <*> pProposalTxOutRef

pCreateNewActiveInfoAuto :: Parser Command
pCreateNewActiveInfoAuto =
    CreateDatum
      <$> pNewActive
      <*> pOutputFile
  where
    pNewActive :: Parser NewDatum
    pNewActive =
      NewActiveDatumAuto
        <$> pNetwork
        <*> pApiService
        <*> (fromIntegral <$> pDesiredTermsIndex)
        <*> pProposalTxOutRef

pCreatePostAddressUpdateActive :: Parser Command
pCreatePostAddressUpdateActive = hsubparser $ mconcat
  [ command "manual" $
      info pCreatePostAddressUpdateActiveManual $ 
        progDesc "Create a post-address-update ActiveDatum manually."
  , command "auto" $
      info pCreatePostAddressUpdateActiveAuto $ 
        progDesc "Create a post-address-update ActiveDatum by looking up the contract UTxO."
  ]

pCreatePostAddressUpdateActiveManual :: Parser Command
pCreatePostAddressUpdateActiveManual =
    CreateDatum
      <$> pNewAddressUpdateInfo
      <*> pOutputFile
  where
    pNewAddressUpdateInfo :: Parser NewDatum
    pNewAddressUpdateInfo =
      fmap NewPostAddressUpdateActiveDatumManual $ NewAddressInfo
        <$> pOfferAsset
        <*> pOfferQuantity
        <*> pAskAsset
        <*> pStrikePrice
        <*> pExpiration
        <*> pContractDeposit
        <*> pPaymentAddress
        <*> pContractId

pCreatePostAddressUpdateActiveAuto :: Parser Command
pCreatePostAddressUpdateActiveAuto =
    CreateDatum
      <$> pNewAddressUpdate
      <*> pOutputFile
  where
    pNewAddressUpdate :: Parser NewDatum
    pNewAddressUpdate =
      NewPostAddressUpdateActiveDatumAuto
        <$> pNetwork
        <*> pApiService
        <*> pContractTxOutRef
        <*> pPaymentAddress

-------------------------------------------------
-- CreateRedeemer Parser
-------------------------------------------------
parseCreateRedeemer :: Parser Command
parseCreateRedeemer = hsubparser $ mconcat
    [ command "proposal-script" $
        info pProposalRedeemer $ progDesc "Create a redeemer for the proposal script."
    , command "options-script" $
        info pOptionsRedeemer $ progDesc "Create a redeemer for the options script."
    , command "active-script" $
        info pActiveRedeemer $ progDesc "Create a redeemer for the active script."
    , command "address-update-script" $
        info pAddressObserverRedeemer $ 
          progDesc "Create a redeemer for the address update observer script."
    ]

pProposalRedeemer :: Parser Command
pProposalRedeemer = hsubparser $ mconcat
    [ command "manage-proposals" $
        info pCreateCloseOrUpdateProposals $ 
          progDesc "Create the redeemer for creating/updating/closing Proposal UTxOs."
    , command "burn-all" $
        info pBurnProposalBeacons $ progDesc "Create the redeemer for burning all beacons."
    , command "register" $
        info pRegisterProposalScript $ progDesc "Create the redeemer for registering the script."
    ]
  where
    pCreateCloseOrUpdateProposals :: Parser Command
    pCreateCloseOrUpdateProposals = 
      CreateRedeemer
        <$> pure (NewProposalRedeemer CreateCloseOrUpdateProposals)
        <*> pOutputFile

    pBurnProposalBeacons :: Parser Command
    pBurnProposalBeacons = 
      CreateRedeemer 
        <$> pure (NewProposalRedeemer BurnProposalBeacons)
        <*> pOutputFile

    pRegisterProposalScript :: Parser Command
    pRegisterProposalScript =
      CreateRedeemer 
        <$> pure (NewProposalRedeemer RegisterProposalScript)
        <*> pOutputFile

pOptionsRedeemer :: Parser Command
pOptionsRedeemer = hsubparser $ mconcat
    [ command "manage-proposal" $
        info pCloseOrUpdateProposal $ 
          progDesc "Create the redeemer for updating/closing Proposal UTxOs."
    , command "purchase" $
        info pPurchaseContract $ 
          progDesc "Create the redeemer for purchasing a Proposal UTxO."
    , command "execute" $
        info pExecute $ 
          progDesc "Create the redeemer for executing an Active UTxO."
    , command "close-expired" $
        info pCloseExpired $ 
          progDesc "Create the redeemer for closing an expired Active UTxO."
    , command "update-payment-address" $
        info pUpdatePaymentAddress $ 
          progDesc "Create the redeemer for changing the payment address."
    ]
  where
    pCloseOrUpdateProposal :: Parser Command
    pCloseOrUpdateProposal = 
      CreateRedeemer
        <$> pure (NewOptionsRedeemer CloseOrUpdateProposal)
        <*> pOutputFile

    pPurchaseContract :: Parser Command
    pPurchaseContract = 
      CreateRedeemer 
        <$> (NewOptionsRedeemer . PurchaseContract <$> pDesiredTermsIndex) 
        <*> pOutputFile

    pExecute :: Parser Command
    pExecute = 
      CreateRedeemer
        <$> pure (NewOptionsRedeemer ExecuteContract)
        <*> pOutputFile

    pCloseExpired :: Parser Command
    pCloseExpired = 
      CreateRedeemer 
        <$> pure (NewOptionsRedeemer CloseExpiredContract) 
        <*> pOutputFile

    pUpdatePaymentAddress :: Parser Command
    pUpdatePaymentAddress = 
      CreateRedeemer
        <$> (fmap NewOptionsRedeemer . UpdatePaymentAddress <$> pPaymentAddress <*> pDepositIncrease)
        <*> pOutputFile

pActiveRedeemer :: Parser Command
pActiveRedeemer = hsubparser $ mconcat
    [ command "main" $
        info pMain $ 
          progDesc "Create the redeemer for purchasing, executing, or closing contracts"
    , command "burn-all" $
        info pBurnActiveBeacons $ 
          progDesc "Create the redeemer for burning active beacons."
    ]
  where
    pMain :: Parser Command
    pMain = 
      CreateRedeemer
        <$> pure (NewActiveRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol)
        <*> pOutputFile

    pBurnActiveBeacons :: Parser Command
    pBurnActiveBeacons = 
      CreateRedeemer
        <$> pure (NewActiveRedeemer BurnActiveBeacons)
        <*> pOutputFile

pAddressObserverRedeemer :: Parser Command
pAddressObserverRedeemer = hsubparser $ mconcat
    [ command "observe-address-update" $
        info pObserveAddressUpdate $ 
          progDesc "Create the redeemer for observing a payment address update."
    , command "register" $
        info pRegisterAddressObserverScript $ 
          progDesc "Create the redeemer for registering the script."
    ]
  where
    pObserveAddressUpdate :: Parser Command
    pObserveAddressUpdate = 
      CreateRedeemer 
        <$> pure (NewAddressObserverRedeemer ObserveAddressUpdate)
        <*> pOutputFile

    pRegisterAddressObserverScript :: Parser Command
    pRegisterAddressObserverScript =
      CreateRedeemer 
        <$> pure (NewAddressObserverRedeemer RegisterAddressObserverScript)
        <*> pOutputFile

-------------------------------------------------
-- Beacon Name Parsers
-------------------------------------------------
parseBeaconName :: Parser Command
parseBeaconName = hsubparser $ mconcat
    [ command "policy-id" $
        info pPolicyId $ progDesc "Calculate a beacon policy id."
    , command "asset-name" $
        info pAssetName $ progDesc "Calculate a beacon asset name."
    ]
  where
    pProposalPolicyId :: Parser BeaconName
    pProposalPolicyId = flag' ProposalPolicyId
      (  long "proposal-beacons"
      <> help "Calculate the policy id for the proposal beacons."
      )

    pActiveId :: Parser BeaconName
    pActiveId = flag' ActivePolicyId
      (  long "active-beacons"
      <> help "Calculate the policy id for the active beacons."
      )

    pOfferBeaconName :: Parser Command
    pOfferBeaconName = 
      BeaconName 
        <$> (OfferBeaconName <$> pOfferAsset)
        <*> pOutput

    pAskBeaconName :: Parser Command
    pAskBeaconName = 
      BeaconName 
        <$> (AskBeaconName <$> pAskAsset)
        <*> pOutput

    pPremiumBeaconName :: Parser Command
    pPremiumBeaconName = 
      BeaconName
        <$> (PremiumBeaconName <$> pPremiumAsset)
        <*> pOutput

    pTradingPairBeaconName :: Parser Command
    pTradingPairBeaconName = 
      BeaconName
        <$> (TradingPairBeaconName <$> pOfferAsset <*> pAskAsset)
        <*> pOutput

    pContractIdName :: Parser Command
    pContractIdName = 
      BeaconName 
        <$> (ContractIdName <$> pProposalTxOutRef)
        <*> pOutput

    pAssetName :: Parser Command
    pAssetName = hsubparser $ mconcat
      [ command "offer-beacon" $
          info pOfferBeaconName $ progDesc "Calculate the name for the Offer Beacon."
      , command "ask-beacon" $
          info pAskBeaconName $ progDesc "Calculate the name for the Ask Beacon."
      , command "premium-beacon" $
          info pPremiumBeaconName $ progDesc "Calculate the name for the Premium Beacon."
      , command "trading-pair-beacon" $
          info pTradingPairBeaconName $ progDesc "Calculate the name for the TradingPair Beacon."
      , command "contract-id" $
          info pContractIdName $ progDesc "Calculate the name for the Contract ID."
      ]

    pPolicyId :: Parser Command
    pPolicyId = 
      BeaconName
        <$> (pProposalPolicyId <|> pActiveId)
        <*> pOutput

-------------------------------------------------
-- ConvertTime Parser
-------------------------------------------------
pConvertTime :: Parser Command
pConvertTime = ConvertTime <$> (pPOSIXTime <|> pSlot) <*> pNetwork
  where
    pPOSIXTime :: Parser ConvertTime
    pPOSIXTime = POSIXTimeToSlot . POSIXTime <$> option auto
      (  long "posix-time"
      <> metavar "INT"
      <> help "Convert POSIX time (in milliseconds) to slot number."
      )

    pSlot :: Parser ConvertTime
    pSlot = SlotToPOSIXTime . Slot <$> option auto
      (  long "slot"
      <> metavar "INT"
      <> help "Convert slot number to POSIX time."
      )

-------------------------------------------------
-- Submit Parser
-------------------------------------------------
pSubmitTx :: Parser Command
pSubmitTx = 
  SubmitTx
    <$> pNetwork
    <*> pApiService
    <*> pTxFile

-------------------------------------------------
-- EvaluateTx Parser
-------------------------------------------------
pEvaluateTx :: Parser Command
pEvaluateTx = 
  EvaluateTx 
    <$> pNetwork
    <*> pApiService
    <*> pTxFile

-------------------------------------------------
-- ExportParams Parser
-------------------------------------------------
pExportParams :: Parser Command
pExportParams =
  ExportParams
    <$> pNetwork
    <*> pOutput

-------------------------------------------------
-- Query Parser
-------------------------------------------------
parseQuery :: Parser Command
parseQuery = fmap Query . hsubparser $ mconcat
  [ command "personal-address" $
      info pQueryPersonal $ progDesc "Query your personal address." 
  , command "proposals" $
      info pQueryProposals $ progDesc "Query open proposals for the protocol." 
  , command "actives" $
      info pQueryActives $ progDesc "Query active contracts for the protocol." 
  , command "current-slot" $
      info pQueryCurrentSlot $ progDesc "Query the current slot number."
  ]

pQueryPersonal :: Parser Query
pQueryPersonal =
  QueryPersonal
    <$> pNetwork
    <*> pApiService
    <*> pBech32Address
    <*> pKeysOnly
    <*> pFormat
    <*> pOutput
  where
    pKeysOnly :: Parser Bool
    pKeysOnly = flag False True
      (  long "keys"
      <> help "Show only UTxOs with contract key NFTs."
      )

pQueryCurrentSlot :: Parser Query
pQueryCurrentSlot =
  QueryCurrentSlot
    <$> pNetwork
    <*> pApiService

pQueryProposals :: Parser Query
pQueryProposals =
  QueryProposals
    <$> pNetwork
    <*> pApiService
    <*> ((Just <$> pOfferAsset) <|> pure Nothing)
    <*> ((Just <$> pAskAsset) <|> pure Nothing)
    <*> ((Just <$> pPremiumAsset) <|> pure Nothing)
    <*> ((Just <$> pBech32Address) <|> pure Nothing)
    <*> pFormat
    <*> pOutput

pQueryActives :: Parser Query
pQueryActives =
  QueryActives
    <$> pNetwork
    <*> pApiService
    <*> ((Just <$> pOfferAsset) <|> pure Nothing)
    <*> ((Just <$> pAskAsset) <|> pure Nothing)
    <*> ((Just <$> pContractId) <|> pure Nothing)
    <*> ((Just <$> pBech32Address) <|> pure Nothing)
    <*> pFormat
    <*> pOutput

-------------------------------------------------
-- Basic Helper Parsers
-------------------------------------------------
pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "Save to file."
  <> completer (bashCompleter "file")
  )

pNetwork :: Parser Network
pNetwork = pPreProdTestnet <|> pMainnet
  where
    pPreProdTestnet :: Parser Network
    pPreProdTestnet = flag' PreProdTestnet
      (  long "testnet"
      <> help "For the preproduction testnet.")

    pMainnet :: Parser Network
    pMainnet = flag' Mainnet
      (  long "mainnet"
      <> help "For the mainnet.")

pApiService :: Parser ApiService
pApiService = pure Koios
  -- where
  --   pKoios :: Parser Endpoint
  --   pKoios = flag' Koios
  --     (  long "koios"
  --     <> help "Use Koios."
  --     )
  --
  
pContractId :: Parser ContractId
pContractId = ContractId <$> option (eitherReader readTokenName)
  (  long "contract-id"
  <> metavar "STRING"
  <> help "The contract id for this options contract."
  )

pAsset :: String -> Parser (CurrencySymbol,TokenName)
pAsset prefix = option (eitherReader readAsset)
  (  long (prefix <> "-asset")
  <> metavar "STRING"
  <> help ("The " <> prefix <> " asset (lovelace or policy_id.asset_name).")
  )

pOfferAsset :: Parser OfferAsset
pOfferAsset = OfferAsset <$> pAsset "offer"

pAskAsset :: Parser AskAsset
pAskAsset = AskAsset <$> pAsset "ask"

pPremiumAsset :: Parser PremiumAsset
pPremiumAsset = PremiumAsset <$> pAsset "premium"

pOfferQuantity :: Parser Integer
pOfferQuantity = option auto
  (  long "quantity"
  <> metavar "INT"
  <> help "The amount offered."
  )

pContractDeposit :: Parser Integer
pContractDeposit = option auto
  (  long "deposit"
  <> metavar "INT"
  <> help "The amount used for the minUTxOValue."
  )

pPaymentAddress :: Parser Address
pPaymentAddress = 
  option (maybeReader $ rightToMaybe . paymentAddressToPlutusAddress . PaymentAddress . toText)
    (  long "payment-address"
    <> metavar "BECH32"
    <> help "The address where payments must go."
    )

pPremium :: Parser Integer
pPremium = option auto
  (  long "premium"
  <> metavar "INT"
  <> help "The amount requested for the premium."
  )

pStrikePrice :: Parser Fraction
pStrikePrice = option (eitherReader readFraction)
  (  long "strike-price"
  <> metavar "FRACTION"
  <> help "The strike price for the contract."
  )

pExpiration :: Parser POSIXTime
pExpiration = POSIXTime <$> option auto
  (  long "expiration"
  <> metavar "TIME"
  <> help "The expiration time for the options contract in POSIX time (milliseconds)."
  )

pPossibleTerm :: Parser Terms
pPossibleTerm =
  Terms
    <$> pPremium
    <*> pStrikePrice
    <*> pExpiration

pDepositIncrease :: Parser Integer
pDepositIncrease = option auto
  (  long "deposit-increase"
  <> metavar "INT"
  <> help "The ada added for the minUTxOValue increase."
  )

pDesiredTermsIndex :: Parser Integer
pDesiredTermsIndex = option auto
  (  long "desired-terms-index"
  <> metavar "INT"
  <> help "The 0-based index for the desired terms."
  )

pProposalTxOutRef :: Parser TxOutRef
pProposalTxOutRef = option (eitherReader readTxOutRef)
  (  long "proposal-ref"
  <> metavar "STRING"
  <> help "The output reference for the corresponding proposal input 'tx_hash#index'."
  )

pContractTxOutRef :: Parser TxOutRef
pContractTxOutRef = option (eitherReader readTxOutRef)
  (  long "contract-ref"
  <> metavar "STRING"
  <> help "The output reference for the corresponding proposal input 'tx_hash#index'."
  )

pOutput :: Parser Output
pOutput = pStdOut <|> File <$> pOutputFile
  where
    pStdOut :: Parser Output
    pStdOut = flag' Stdout
      (  long "stdout"
      <> help "Display to stdout."
      )

pTxFile :: Parser FilePath
pTxFile = strOption
  (  long "tx-file"
  <> metavar "STRING"
  <> help "Transaction file path."
  )

pFormat :: Parser Format
pFormat = pJSON <|> pPretty <|> pPlain
  where
    pJSON :: Parser Format
    pJSON = flag' JSON
      (  long "json"
      <> help "Format as JSON."
      )

    pPretty :: Parser Format
    pPretty = flag' Pretty
      (  long "pretty"
      <> help "Format for pretty-printing."
      )

    pPlain :: Parser Format
    pPlain = flag' Plain
      (  long "plain"
      <> help "Format for pretty-printing without colors."
      )

pBech32Address :: Parser PaymentAddress
pBech32Address = PaymentAddress <$> strOption
    (  long "address"
    <> metavar "BECH32"
    <> help "The target address."
    )
