{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.ProposalUTxOs.CloseProposal.Failures where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)

import CardanoOptions

import Test.Prelude

-------------------------------------------------
-- Beacon Failures
-------------------------------------------------
-- | Close a single valid Proposal UTxO, but withdraw the beacons instead of burning them. The
-- proposal beacon script is not executed. The Proposal UTxO is spent using CloseOrUpdateProposal.
beaconFailure1 :: MonadEmulator m => m ()
beaconFailure1 = do
  let -- Writer Info
      writerWallet = Mock.knownMockWallet 1
      writerPersonalAddr = Mock.mockWalletAddress writerWallet
      writerPayPrivKey = Mock.paymentPrivateKey writerWallet
      writerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash writerWallet
      writerCred = PV2.PubKeyCredential writerPubKey
      optionsAddress = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential optionsScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash writerCred
        }

      -- Contract Info
      proposalDatum = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (adaSymbol,adaToken)
        , offerQuantity = 10_000_000
        , askAsset = AskAsset (testTokenSymbol,"TestToken1")
        , premiumAsset = PremiumAsset (adaSymbol,adaToken)
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 1000
                , strikePrice = Fraction (1,1_000_000)
                , premium = 2_000_000
                }
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the Proposal UTxO.
  void $ transact writerPersonalAddr [refScriptAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [proposalDatum] $ \ProposalDatum{..} ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unPremiumBeacon premiumBeacon, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateProposals
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          ]
      , outputs = flip map [proposalDatum] $ \datum@ProposalDatum{..} ->
          Output
            { outputAddress = optionsAddress
            , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                [ PV2.singleton proposalBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                , PV2.singleton proposalBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                , PV2.singleton proposalBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                , PV2.singleton proposalBeaconCurrencySymbol (unPremiumBeacon premiumBeacon) 1
                , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [proposalBeaconsRef]
      , extraKeyWitnesses = [writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 1000
          }
      }

  proposalUTxOs <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress

  -- Try to close the Proposal UTxO.
  void $ transact writerPersonalAddr [optionsAddress,refScriptAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens = [ ]
      , inputs = flip map proposalUTxOs $ \(proposalRef,_) ->
          Input
            { inputId = proposalRef
            , inputWitness = 
                SpendWithPlutusReference optionsRef InlineDatum $ toRedeemer CloseOrUpdateProposal
            }
      , referenceInputs = [proposalBeaconsRef,optionsRef]
      , extraKeyWitnesses = [writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Nothing
          }
      }

-- | Close a single valid Proposal UTxO, but withdraw the beacons instead of burning them. The
-- proposal beacon script is still executed.
beaconFailure2 :: MonadEmulator m => m ()
beaconFailure2 = do
  let -- Writer Info
      writerWallet = Mock.knownMockWallet 1
      writerPersonalAddr = Mock.mockWalletAddress writerWallet
      writerPayPrivKey = Mock.paymentPrivateKey writerWallet
      writerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash writerWallet
      writerCred = PV2.PubKeyCredential writerPubKey
      optionsAddress = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential optionsScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash writerCred
        }

      -- Contract Info
      proposalDatum = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (adaSymbol,adaToken)
        , offerQuantity = 10_000_000
        , askAsset = AskAsset (testTokenSymbol,"TestToken1")
        , premiumAsset = PremiumAsset (adaSymbol,adaToken)
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 1000
                , strikePrice = Fraction (1,1_000_000)
                , premium = 2_000_000
                }
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the Proposal UTxO.
  void $ transact writerPersonalAddr [refScriptAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [proposalDatum] $ \ProposalDatum{..} ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unPremiumBeacon premiumBeacon, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateProposals
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          ]
      , outputs = flip map [proposalDatum] $ \datum@ProposalDatum{..} ->
          Output
            { outputAddress = optionsAddress
            , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                [ PV2.singleton proposalBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                , PV2.singleton proposalBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                , PV2.singleton proposalBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                , PV2.singleton proposalBeaconCurrencySymbol (unPremiumBeacon premiumBeacon) 1
                , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [proposalBeaconsRef]
      , extraKeyWitnesses = [writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 1000
          }
      }

  proposalUTxOs <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress

  -- Try to close the Proposal UTxO.
  void $ transact writerPersonalAddr [optionsAddress,refScriptAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposalUTxOs $ \(_, Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unPremiumBeacon premiumBeacon, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateProposals
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          ]
      , inputs = flip map proposalUTxOs $ \(proposalRef,_) ->
          Input
            { inputId = proposalRef
            , inputWitness = 
                SpendWithPlutusReference optionsRef InlineDatum $ toRedeemer CloseOrUpdateProposal
            }
      , outputs = flip map [proposalDatum] $ \datum@ProposalDatum{..} ->
          Output
            { outputAddress = optionsAddress
            , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                [ PV2.singleton proposalBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                , PV2.singleton proposalBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                , PV2.singleton proposalBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                , PV2.singleton proposalBeaconCurrencySymbol (unPremiumBeacon premiumBeacon) 1
                , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [proposalBeaconsRef,optionsRef]
      , extraKeyWitnesses = [writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 1000
          }
      }

-------------------------------------------------
-- Datum Failures
-------------------------------------------------
-- | Close a UTxO that does not have a ProposalDatum.
datumFailure1 :: MonadEmulator m => m ()
datumFailure1 = do
  let -- Writer Info
      writerWallet = Mock.knownMockWallet 1
      writerPersonalAddr = Mock.mockWalletAddress writerWallet
      writerPayPrivKey = Mock.paymentPrivateKey writerWallet
      writerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash writerWallet
      writerCred = PV2.PubKeyCredential writerPubKey
      optionsAddress = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential optionsScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash writerCred
        }

      -- Contract Info
      proposalDatum@ProposalDatum{..} = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (adaSymbol,adaToken)
        , offerQuantity = 10_000_000
        , askAsset = AskAsset (testTokenSymbol,"TestToken1")
        , premiumAsset = PremiumAsset (adaSymbol,adaToken)
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 1000
                , strikePrice = Fraction (1,1_000_000)
                , premium = 2_000_000
                }
            ]
        }

      activeDatum = createActiveDatumFromProposal 0 (TxOutRef (TxId "") 1) proposalDatum

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the Proposal UTxO.
  void $ transact writerPersonalAddr [refScriptAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens = [ ]
      , outputs =
          [ Output
              { outputAddress = optionsAddress
              , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                  [ uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = []
      , extraKeyWitnesses = [writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 1000
          }
      }

  utxo <- txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress

  -- Try to close the UTxO.
  void $ transact writerPersonalAddr [optionsAddress,refScriptAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens = [ ]
      , inputs = flip map utxo $ \(proposalRef,_) ->
          Input
            { inputId = proposalRef
            , inputWitness = 
                SpendWithPlutusReference optionsRef InlineDatum $ toRedeemer CloseOrUpdateProposal
            }
      , referenceInputs = [optionsRef]
      , extraKeyWitnesses = [writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Nothing
          }
      }

-------------------------------------------------
-- Approval Failures
-------------------------------------------------
-- | Close a single valid Proposal UTxO, but the writer did not approve.
approvalFailure1 :: MonadEmulator m => m ()
approvalFailure1 = do
  let -- Writer1 Info
      writerWallet1 = Mock.knownMockWallet 1
      writerPersonalAddr1 = Mock.mockWalletAddress writerWallet1
      writerPayPrivKey1 = Mock.paymentPrivateKey writerWallet1
      writerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash writerWallet1
      writerCred1 = PV2.PubKeyCredential writerPubKey1
      optionsAddress1 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential optionsScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash writerCred1
        }

      -- Writer2 Info
      writerWallet2 = Mock.knownMockWallet 2
      -- writerPersonalAddr2 = Mock.mockWalletAddress writerWallet2
      -- writerPayPrivKey2 = Mock.paymentPrivateKey writerWallet2
      writerPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash writerWallet2
      writerCred2 = PV2.PubKeyCredential writerPubKey2
      optionsAddress2 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential optionsScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash writerCred2
        }

      -- Contract Info
      proposalDatum@ProposalDatum{..} = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (adaSymbol,adaToken)
        , offerQuantity = 10_000_000
        , askAsset = AskAsset (testTokenSymbol,"TestToken1")
        , premiumAsset = PremiumAsset (adaSymbol,adaToken)
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr1
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 1000
                , strikePrice = Fraction (1,1_000_000)
                , premium = 2_000_000
                }
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet1 10_000_000 [("TestToken1",1000)]

  -- Try to create the Proposal UTxO.
  void $ transact writerPersonalAddr1 [refScriptAddress] [writerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (unOfferBeacon offerBeacon, 2)
                  , (unAskBeacon askBeacon, 2)
                  , (unTradingPairBeacon tradingPairBeacon, 2)
                  , (unPremiumBeacon premiumBeacon, 2)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateProposals
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          ]
      , outputs = 
          [ Output
              { outputAddress = optionsAddress1
              , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                  [ PV2.singleton proposalBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                  , PV2.singleton proposalBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                  , PV2.singleton proposalBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                  , PV2.singleton proposalBeaconCurrencySymbol (unPremiumBeacon premiumBeacon) 1
                  , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                  ]
              , outputDatum = OutputDatum $ toDatum proposalDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = optionsAddress2
              , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                  [ PV2.singleton proposalBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                  , PV2.singleton proposalBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                  , PV2.singleton proposalBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                  , PV2.singleton proposalBeaconCurrencySymbol (unPremiumBeacon premiumBeacon) 1
                  , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                  ]
              , outputDatum = OutputDatum $ toDatum proposalDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [proposalBeaconsRef]
      , extraKeyWitnesses = [writerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 1000
          }
      }

  proposalUTxOs <- concat <$>
    mapM (txOutRefsAndDatumsAtAddress @ProposalDatum) [optionsAddress2]

  -- Try to close the Proposal UTxO.
  void $ transact 
    writerPersonalAddr1 
    [optionsAddress1,optionsAddress2,refScriptAddress] 
    [writerPayPrivKey1]
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposalUTxOs $ \(_, _) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateProposals
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          ]
      , inputs = flip map proposalUTxOs $ \(proposalRef,_) ->
          Input
            { inputId = proposalRef
            , inputWitness = 
                SpendWithPlutusReference optionsRef InlineDatum $ toRedeemer CloseOrUpdateProposal
            }
      , referenceInputs = [proposalBeaconsRef,optionsRef]
      , extraKeyWitnesses = [writerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Nothing
          }
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all failure scenarios for closing Proposal UTxOs.
tests :: [TestTree]
tests =
  [ -- Beacon Failures
    scriptMustFailWithError "beaconFailure1" 
      "Proposal beacon script not executed with proper redeemer"
      beaconFailure1
  , scriptMustFailWithError "beaconFailure2" 
      "Beacon must go to a DApp address using a valid staking credential"
      beaconFailure2

    -- Datum Failures
  , scriptMustFailWithError "datumFailure1" 
      "UTxO is not a Proposal UTxO"
      datumFailure1

    -- Approval Failures
  , scriptMustFailWithError "approvalFailure1" 
      "Writer did not approve"
      approvalFailure1
  ]
