{-# LANGUAGE TemplateHaskell #-}

module CLI.Run
  (
    runCommand
  ) where

import Relude
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Prettyprinter
import Prettyprinter.Render.Terminal
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.ByteString as SBS
import Data.FileEmbed
import Optics.Operators

import CardanoOptions

import CLI.Data.Bech32Address
import CLI.Data.Commands
import CLI.Data.Network
import CLI.Data.OptionsUTxO
import CLI.Data.Output
import CLI.Data.PersonalUTxO
import CLI.Query

preprodParams :: SBS.ByteString
preprodParams = $(embedFile "preprod-params.json")

mainnetParams :: SBS.ByteString
mainnetParams = $(embedFile "mainnet-params.json")

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ExportScript script file -> runExportScript script file
  CreateDatum protocolDatum file -> runCreateDatum protocolDatum file
  CreateRedeemer newRedeemer file -> runCreateRedeemer newRedeemer file
  BeaconName info output -> runBeaconName info output
  Query query -> runQuery query
  ConvertTime convert network -> runTimeConversion convert network
  SubmitTx network api txFile ->
    runSubmitTx network api txFile >>= LBS.putStr . encode
  EvaluateTx network api txFile ->
    runEvaluateTx network api txFile >>= LBS.putStr . encode
  ExportParams network output -> runExportParams network output

runExportParams :: Network -> Output -> IO ()
runExportParams network output = case (network,output) of
  (PreProdTestnet,Stdout) -> SBS.putStr preprodParams
  (PreProdTestnet,File file) -> SBS.writeFile file preprodParams
  (Mainnet,Stdout) -> SBS.putStr mainnetParams
  (Mainnet,File file) -> SBS.writeFile file mainnetParams

runExportScript :: Script -> FilePath -> IO ()
runExportScript script file = do
  flip whenLeftM_ (\e -> putStrLn $ "There was an error: " <> show e) $
    writeScript file $ case script of
      ProposalBeaconScript -> proposalBeaconScript
      ActiveBeaconScript -> activeBeaconScript
      AddressUpdateObserverScript -> addressObserverScript
      OptionsScript -> optionsScript
      ProxyScript -> proxyScript

runTimeConversion :: ConvertTime -> Network -> IO ()
runTimeConversion time network = case time of
    POSIXTimeToSlot p -> print $ getSlot $ posixTimeToSlot config p
    SlotToPOSIXTime s -> print $ getPOSIXTime $ slotToPOSIXTime config s
  where
    config = case network of
      Mainnet -> mainnetConfig
      PreProdTestnet -> preprodConfig

runCreateDatum :: NewDatum -> FilePath -> IO ()
runCreateDatum (NewProposalDatum newProposalInfo) file = 
  writeData file $ unsafeCreateProposalDatum newProposalInfo
runCreateDatum (NewPaymentDatum contractId) file = 
  writeData file $ PaymentDatum (ActiveBeaconId activeBeaconCurrencySymbol,contractId)
runCreateDatum (NewActiveDatumManual newActive) file = 
  writeData file $ unsafeCreateActiveDatum newActive
runCreateDatum (NewActiveDatumAuto network endpoint desiredTermsIndex proposalRef) file = do
  utxo <- runQuerySpecificOptionsUTxO network endpoint proposalRef
  case utxo of
    [OptionsUTxO{optionsDatum=Just (Proposal datum)}] -> 
      writeData file $ createActiveDatumFromProposal desiredTermsIndex proposalRef datum
    _ -> error "Not a Proposal UTxO."
runCreateDatum (NewPostAddressUpdateActiveDatumManual datum) file = 
  writeData file $ unsafeCreatePostAddressUpdateActiveDatum datum
runCreateDatum (NewPostAddressUpdateActiveDatumAuto network endpoint contractRef newAddr) file = do
  utxo <- runQuerySpecificOptionsUTxO network endpoint contractRef
  case utxo of
    [OptionsUTxO{optionsDatum=Just (Active datum)}] -> 
      writeData file $ datum & #paymentAddress .~ newAddr
    _ -> error "Not an Active UTxO."

runCreateRedeemer :: NewRedeemer -> FilePath -> IO ()
runCreateRedeemer (NewProposalRedeemer redeemer) file = writeData file redeemer
runCreateRedeemer (NewOptionsRedeemer redeemer) file = writeData file redeemer
runCreateRedeemer (NewActiveRedeemer redeemer) file = writeData file redeemer
runCreateRedeemer (NewAddressObserverRedeemer redeemer) file = writeData file redeemer

runBeaconName :: BeaconName -> Output -> IO ()
runBeaconName name output = 
    displayName $ case name of
      ProposalPolicyId -> show proposalBeaconCurrencySymbol
      ActivePolicyId -> show activeBeaconCurrencySymbol
      OfferBeaconName asset -> 
        showTokenName $ unOfferBeacon $ genOfferBeaconName asset
      AskBeaconName asset -> 
        showTokenName $ unAskBeacon $ genAskBeaconName asset
      PremiumBeaconName asset -> 
        showTokenName $ unPremiumBeacon $ genPremiumBeaconName asset
      TradingPairBeaconName offerAsset askAsset -> 
        showTokenName $ unTradingPairBeacon $ genTradingPairBeaconName offerAsset askAsset
      ContractIdName proposalRef -> 
        showTokenName $ unContractId $ genContractId proposalRef
  where
    displayName :: String -> IO ()
    displayName = case output of
      Stdout -> putStr
      File file -> writeFile file

runQuery :: Query -> IO ()
runQuery query = case query of
  QueryCurrentSlot network api -> runQuerySlotTip network api >>= print
  QueryPersonal network api addr keysOnly format output ->
    runQueryPersonalAddress network api addr keysOnly >>= 
      case format of
        JSON -> toJSONOutput output
        Pretty -> toPrettyOutput output 
                . (<> hardline) 
                . (personalHeader <>) 
                . vsep 
                . map prettyPersonalUTxO 
        Plain -> toPlainOutput output 
                . (<> hardline) 
                . (personalHeader <>) 
                . vsep 
                . map prettyPersonalUTxO
  QueryProposals network api mOfferAsset mAskAsset mPremiumAsset mWriterAddr format output -> do
    let askBeacon = 
          ((proposalBeaconCurrencySymbol,) . unAskBeacon . genAskBeaconName)
            <$> mAskAsset
        offerBeacon = 
          ((proposalBeaconCurrencySymbol,) . unOfferBeacon . genOfferBeaconName)
            <$> mOfferAsset
        premiumBeacon = 
          ((proposalBeaconCurrencySymbol,) . unPremiumBeacon . genPremiumBeaconName)
            <$> mPremiumAsset
        pairBeacon = fmap ((proposalBeaconCurrencySymbol,) . unTradingPairBeacon) 
                   . genTradingPairBeaconName <$> mOfferAsset <*> mAskAsset
        assets
          | isJust pairBeacon = catMaybes [pairBeacon,premiumBeacon]
          | otherwise = catMaybes [askBeacon,offerBeacon,premiumBeacon]
    case (mWriterAddr,assets) of
      (Nothing,[]) -> 
        print @Text "At least one beacon filter must be specified or a writer address must be specified."
      _ -> runQueryOptionsUTxOs network api assets mWriterAddr >>= case format of
        JSON -> toJSONOutput output
        Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettyOptionsUTxO network)
        Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettyOptionsUTxO network)
  QueryActives network api mOfferAsset mAskAsset mContractId mWriterAddr format output -> do
    let askBeacon = 
          ((activeBeaconCurrencySymbol,) . unAskBeacon . genAskBeaconName)
            <$> mAskAsset
        offerBeacon = 
          ((activeBeaconCurrencySymbol,) . unOfferBeacon . genOfferBeaconName)
            <$> mOfferAsset
        contractIdBeacon = 
          ((activeBeaconCurrencySymbol,) . unContractId)
            <$> mContractId
        pairBeacon = fmap ((activeBeaconCurrencySymbol,) . unTradingPairBeacon) 
                   . genTradingPairBeaconName <$> mOfferAsset <*> mAskAsset
        assets
          | isJust pairBeacon = catMaybes [pairBeacon,contractIdBeacon]
          | otherwise = catMaybes [askBeacon,offerBeacon,contractIdBeacon]
    case (mWriterAddr,assets) of
      (Nothing,[]) -> 
        print @Text "At least one beacon filter must be specified or a writer address must be specified."
      (Nothing,_) -> do
        rawResult <- runQueryOptionsUTxOs network api assets mWriterAddr
        let result
              | catMaybes [offerBeacon,askBeacon] == [] = flip filter rawResult $
                  \OptionsUTxO{optionsAddress} -> fromRight False $ isOptionsAddress optionsAddress
              | otherwise = rawResult
        case format of
          JSON -> toJSONOutput output result
          Pretty -> toPrettyOutput output $ (<> hardline) $ vsep $ map (prettyOptionsUTxO network) result
          Plain -> toPlainOutput output $ (<> hardline) $ vsep $ map (prettyOptionsUTxO network) result
      _ -> runQueryOptionsUTxOs network api assets mWriterAddr >>= case format of
        JSON -> toJSONOutput output
        Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettyOptionsUTxO network)
        Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettyOptionsUTxO network)

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toPlainOutput :: Output -> Doc AnsiStyle -> IO ()
toPlainOutput Stdout xs = TIO.putStr $ T.pack $ show $ unAnnotate xs
toPlainOutput (File file) xs = TIO.writeFile file $ T.pack $ show xs

toPrettyOutput :: Output -> Doc AnsiStyle -> IO ()
toPrettyOutput Stdout xs = putDoc xs
toPrettyOutput (File file) xs = 
  TIO.writeFile file $ renderStrict $ layoutPretty defaultLayoutOptions xs

toJSONOutput :: (ToJSON a) => Output -> [a] -> IO ()
toJSONOutput Stdout xs = LBS.putStr $ encode xs
toJSONOutput (File file) xs = LBS.writeFile file $ encode xs
