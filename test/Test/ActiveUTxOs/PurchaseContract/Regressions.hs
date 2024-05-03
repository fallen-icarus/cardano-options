{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.ActiveUTxOs.PurchaseContract.Regressions where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)

import CardanoOptions

import Test.Prelude

-------------------------------------------------
-- Basic Regression Tests
-------------------------------------------------
-- | Purchase a single valid Proposal UTxO. The Proposal UTxO has only one possible `Terms`.
-- The contract output is first.
regressionTest1 :: MonadEmulator m => m ()
regressionTest1 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

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
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              [ Output
                  { outputAddress = optionsAddress
                  , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                      , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                      ]
                  , outputDatum = OutputDatum $ toDatum datum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = toCardanoApiAddress paymentAddress
                  , outputValue = utxoValue 3_000_000 $ mconcat
                      [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                  , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase a single valid Proposal UTxO. The Proposal UTxO has three possible `Terms`.
-- The contract output is first.
regressionTest2 :: MonadEmulator m => m ()
regressionTest2 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

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
            , Terms
                { expiration = slotToPosixTime 2000
                , strikePrice = Fraction (1,1_000_000)
                , premium = 3_000_000
                }
            , Terms
                { expiration = slotToPosixTime 3000
                , strikePrice = Fraction (1,1_000_000)
                , premium = 4_000_000
                }
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              [ Output
                  { outputAddress = optionsAddress
                  , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                      , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                      ]
                  , outputDatum = OutputDatum $ toDatum datum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = toCardanoApiAddress paymentAddress
                  , outputValue = utxoValue 3_000_000 $ mconcat
                      [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                  , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase multiple valid Proposal UTxOs. The Proposal UTxOs are all for the same conditions, and
-- they all only have one possible `Terms`. The contract output and premium output are paired up;
-- the contract outputs are first.
regressionTest3 :: MonadEmulator m => m ()
regressionTest3 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

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
              { mintTokens = flip concatMap (replicate 3 proposalDatum) $ \ProposalDatum{..} ->
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
      , outputs = flip map (replicate 3 proposalDatum) $ \datum@ProposalDatum{..} ->
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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              [ Output
                  { outputAddress = optionsAddress
                  , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                      , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                      ]
                  , outputDatum = OutputDatum $ toDatum datum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = toCardanoApiAddress paymentAddress
                  , outputValue = utxoValue 3_000_000 $ mconcat
                      [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                  , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase multiple valid Proposal UTxOs. The Proposal UTxOs are all for different conditions, and
-- they all only have one possible `Terms`. The contract output and premium output are paired up;
-- the contract outputs are first.
regressionTest4 :: MonadEmulator m => m ()
regressionTest4 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Contract Info
      proposalDatum1 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken1")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken2")
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
      proposalDatum2 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 1000
                , strikePrice = Fraction (1,1_000_000)
                , premium = 20
                }
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    , ("TestToken4",1000)
    ]
  mintTestTokens buyerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    , ("TestToken4",1000)
    ]

  -- Try to create the Proposal UTxO.
  void $ transact writerPersonalAddr [refScriptAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [proposalDatum1,proposalDatum2] $ \ProposalDatum{..} ->
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
      , outputs = flip map [proposalDatum1,proposalDatum2] $ \datum@ProposalDatum{..} ->
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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              [ Output
                  { outputAddress = optionsAddress
                  , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                      , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                      ]
                  , outputDatum = OutputDatum $ toDatum datum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = toCardanoApiAddress paymentAddress
                  , outputValue = utxoValue 3_000_000 $ mconcat
                      [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                  , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase multiple valid Proposal UTxOs that are stored at different writer addresses. The
-- contract output and premium output are paired up; the contract outputs are first.
regressionTest5 :: MonadEmulator m => m ()
regressionTest5 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 3
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Contract Info
      proposalDatum = unsafeCreateProposalDatum $ NewProposalInfo
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
  mintTestTokens writerWallet2 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the Proposal UTxO.
  void $ transact writerPersonalAddr1 [refScriptAddress] [writerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [proposalDatum] $ \ProposalDatum{..} ->
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
      , outputs = flip concatMap [proposalDatum] $ \ProposalDatum{..} ->
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

  proposals1 <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress1
  proposals2 <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress2

  let desiredTerms1 = zip proposals1 $ repeat (0 :: Int)
      desiredTerms2 = zip proposals2 $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact 
    buyerPersonalAddr 
    [refScriptAddress,optionsAddress1,optionsAddress2] 
    [buyerPayPrivKey]
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals1 $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals1 $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals2 $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals2 $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map (desiredTerms1 <> desiredTerms2) $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = concat
         [ flip concatMap desiredTerms1 $ 
            \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
              let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                  Terms{premium} = possibleTerms !! idx
              in
                [ Output
                    { outputAddress = optionsAddress1
                    , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                        [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                        , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                        , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                        , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                        , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                        ]
                    , outputDatum = OutputDatum $ toDatum datum
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                , Output
                    { outputAddress = toCardanoApiAddress paymentAddress
                    , outputValue = utxoValue 3_000_000 $ mconcat
                        [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                    , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                ]
        , flip concatMap desiredTerms2 $ 
            \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
              let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                  Terms{premium} = possibleTerms !! idx
              in
                [ Output
                    { outputAddress = optionsAddress2
                    , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                        [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                        , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                        , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                        , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                        , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                        ]
                    , outputDatum = OutputDatum $ toDatum datum
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                , Output
                    { outputAddress = toCardanoApiAddress paymentAddress
                    , outputValue = utxoValue 3_000_000 $ mconcat
                        [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                    , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                ]
        ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase a single valid Proposal UTxO. The Proposal UTxO has only one possible `Terms`.
-- The premium output is first.
regressionTest6 :: MonadEmulator m => m ()
regressionTest6 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

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
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              [ Output
                  { outputAddress = toCardanoApiAddress paymentAddress
                  , outputValue = utxoValue 3_000_000 $ mconcat
                      [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                  , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = optionsAddress
                  , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                      , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                      ]
                  , outputDatum = OutputDatum $ toDatum datum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase a single valid Proposal UTxO. The Proposal UTxO has only one possible `Terms`.
-- The premium output is first, but there is an unrelated output between them.
regressionTest7 :: MonadEmulator m => m ()
regressionTest7 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

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
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              [ Output
                  { outputAddress = toCardanoApiAddress paymentAddress
                  , outputValue = utxoValue 3_000_000 $ mconcat
                      [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                  , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = writerPersonalAddr
                  , outputValue = utxoValue 10_000_000 mempty
                  , outputDatum = NoOutputDatum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = optionsAddress
                  , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                      , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                      ]
                  , outputDatum = OutputDatum $ toDatum datum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase a single valid Proposal UTxO. The Proposal UTxO has only one possible `Terms`.
-- The contract output is first, but there is an unrelated output between them.
regressionTest8 :: MonadEmulator m => m ()
regressionTest8 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

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
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              [ Output
                  { outputAddress = optionsAddress
                  , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                      , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                      ]
                  , outputDatum = OutputDatum $ toDatum datum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = writerPersonalAddr
                  , outputValue = utxoValue 10_000_000 mempty
                  , outputDatum = NoOutputDatum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = toCardanoApiAddress paymentAddress
                  , outputValue = utxoValue 3_000_000 $ mconcat
                      [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                  , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase multiple valid Proposal UTxOs. The Proposal UTxOs are all for different conditions, and
-- they all only have one possible `Terms`. The contract output and premium output are separated;
-- the contract outputs are first.
regressionTest9 :: MonadEmulator m => m ()
regressionTest9 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Contract Info
      proposalDatum1 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken1")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken2")
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
      proposalDatum2 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 1000
                , strikePrice = Fraction (1,1_000_000)
                , premium = 20
                }
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    , ("TestToken4",1000)
    ]
  mintTestTokens buyerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    , ("TestToken4",1000)
    ]

  -- Try to create the Proposal UTxO.
  void $ transact writerPersonalAddr [refScriptAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [proposalDatum1,proposalDatum2] $ \ProposalDatum{..} ->
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
      , outputs = flip map [proposalDatum1,proposalDatum2] $ \datum@ProposalDatum{..} ->
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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  let contractOutputs = flip map desiredTerms $ 
          \((ref,Just pd),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
            in
              Output
                { outputAddress = optionsAddress
                , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                    , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }

  let premiumOutputs = flip map desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = contractOutputs <> premiumOutputs
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase multiple valid Proposal UTxOs. The Proposal UTxOs are all for different conditions, and
-- they all only have one possible `Terms`. The contract output and premium output are separated;
-- the premium outputs are first.
regressionTest10 :: MonadEmulator m => m ()
regressionTest10 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Contract Info
      proposalDatum1 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken1")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken2")
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
      proposalDatum2 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 1000
                , strikePrice = Fraction (1,1_000_000)
                , premium = 20
                }
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    , ("TestToken4",1000)
    ]
  mintTestTokens buyerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    , ("TestToken4",1000)
    ]

  -- Try to create the Proposal UTxO.
  void $ transact writerPersonalAddr [refScriptAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [proposalDatum1,proposalDatum2] $ \ProposalDatum{..} ->
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
      , outputs = flip map [proposalDatum1,proposalDatum2] $ \datum@ProposalDatum{..} ->
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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  let contractOutputs = flip map desiredTerms $ 
          \((ref,Just pd),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
            in
              Output
                { outputAddress = optionsAddress
                , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                    , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }

  let premiumOutputs = flip map desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = premiumOutputs <> contractOutputs
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase multiple valid Proposal UTxOs. The Proposal UTxOs are all for different conditions, and
-- they all only have one possible `Terms`. The contract output and premium output are separated;
-- the premium outputs are sandwiched between two groups of contract outputs.
regressionTest11 :: MonadEmulator m => m ()
regressionTest11 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Contract Info
      proposalDatum1 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken1")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken2")
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
      proposalDatum2 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 1000
                , strikePrice = Fraction (1,1_000_000)
                , premium = 20
                }
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    , ("TestToken4",1000)
    ]
  mintTestTokens buyerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    , ("TestToken4",1000)
    ]

  -- Try to create the Proposal UTxO.
  void $ transact writerPersonalAddr [refScriptAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [proposalDatum1,proposalDatum2] $ \ProposalDatum{..} ->
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
      , outputs = flip map [proposalDatum1,proposalDatum2] $ \datum@ProposalDatum{..} ->
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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  let contractOutputs = flip map desiredTerms $ 
          \((ref,Just pd),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
            in
              Output
                { outputAddress = optionsAddress
                , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                    , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }

  let premiumOutputs = flip map desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = take 1 contractOutputs <> premiumOutputs <> drop 1 contractOutputs
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase multiple valid Proposal UTxOs. The Proposal UTxOs are all for different conditions, and
-- they all only have one possible `Terms`. The contract output and premium output are separated;
-- the contract outputs are sandwiched between two groups of premium outputs.
regressionTest12 :: MonadEmulator m => m ()
regressionTest12 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Contract Info
      proposalDatum1 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken1")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken2")
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
      proposalDatum2 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 1000
                , strikePrice = Fraction (1,1_000_000)
                , premium = 20
                }
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    , ("TestToken4",1000)
    ]
  mintTestTokens buyerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    , ("TestToken4",1000)
    ]

  -- Try to create the Proposal UTxO.
  void $ transact writerPersonalAddr [refScriptAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [proposalDatum1,proposalDatum2] $ \ProposalDatum{..} ->
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
      , outputs = flip map [proposalDatum1,proposalDatum2] $ \datum@ProposalDatum{..} ->
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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  let contractOutputs = flip map desiredTerms $ 
          \((ref,Just pd),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
            in
              Output
                { outputAddress = optionsAddress
                , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                    , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }

  let premiumOutputs = flip map desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = take 1 premiumOutputs <> contractOutputs <> drop 1 premiumOutputs
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase a single valid Proposal UTxO. The Proposal UTxO has only one possible `Terms`.
-- The contract output is first. The proposal beacons are used to create a new Proposal UTxO in the
-- same transation. The proposal beacon script is executed as a staking script.
regressionTest13 :: MonadEmulator m => m ()
regressionTest13 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

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
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

      reqOutputs = flip concatMap desiredTerms $ 
        \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
          let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
              Terms{premium} = possibleTerms !! idx
          in
            [ Output
                { outputAddress = optionsAddress
                , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                    , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = concat
          [ reqOutputs
          , flip map [proposalDatum] $ \datum@ProposalDatum{..} ->
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
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash proposalBeaconScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference proposalBeaconsRef $ 
                    toRedeemer CreateCloseOrUpdateProposals
              }
          ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 1000
          }
      }

-- | Purchase a single valid Proposal UTxO. The Proposal UTxO has only one possible `Terms`. The
-- contract output is first. Some of the proposal beacons are used to create a new Proposal UTxO in
-- the same transation. The proposal beacon script is executed as a minting script.
regressionTest14 :: MonadEmulator m => m ()
regressionTest14 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

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
      newProposalDatum = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (adaSymbol,adaToken)
        , offerQuantity = 10_000_000
        , askAsset = AskAsset (testTokenSymbol,"TestToken2")
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
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

      reqOutputs = flip concatMap desiredTerms $ 
        \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
          let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
              Terms{premium} = possibleTerms !! idx
          in
            [ Output
                { outputAddress = optionsAddress
                , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                    , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap [proposalDatum] $ \ProposalDatum{..} ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateProposals
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap [newProposalDatum] $ \ProposalDatum{..} ->
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
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = concat
          [ reqOutputs
          , flip map [newProposalDatum] $ \datum@ProposalDatum{..} ->
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
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash proposalBeaconScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference proposalBeaconsRef $ 
                    toRedeemer CreateCloseOrUpdateProposals
              }
          ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 1000
          }
      }

-- | Purchase a single valid Proposal UTxO. The Proposal UTxO has only one possible `Terms`.
-- The contract output is first. The premium asset is not ada.
regressionTest15 :: MonadEmulator m => m ()
regressionTest15 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Contract Info
      proposalDatum = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (adaSymbol,adaToken)
        , offerQuantity = 10_000_000
        , askAsset = AskAsset (testTokenSymbol,"TestToken1")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken2")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 1000
                , strikePrice = Fraction (1,1_000_000)
                , premium = 2
                }
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken2",1000)]

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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              [ Output
                  { outputAddress = optionsAddress
                  , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                      , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                      ]
                  , outputDatum = OutputDatum $ toDatum datum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = toCardanoApiAddress paymentAddress
                  , outputValue = utxoValue 3_000_000 $ mconcat
                      [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                  , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase a single valid Proposal UTxO. The Proposal UTxO has only one possible `Terms`.
-- The contract output is first. The offer asset is not ada.
regressionTest16 :: MonadEmulator m => m ()
regressionTest16 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Contract Info
      proposalDatum = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
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
  mintTestTokens writerWallet 10_000_000 [("TestToken2",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              [ Output
                  { outputAddress = optionsAddress
                  , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                      , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                      ]
                  , outputDatum = OutputDatum $ toDatum datum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = toCardanoApiAddress paymentAddress
                  , outputValue = utxoValue 3_000_000 $ mconcat
                      [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                  , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase a single valid Proposal UTxO. The Proposal UTxO has only one possible `Terms`.
-- The contract output is first. The offer asset is not ada. More ada is deposited.
regressionTest17 :: MonadEmulator m => m ()
regressionTest17 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Contract Info
      proposalDatum = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
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
  mintTestTokens writerWallet 10_000_000 [("TestToken2",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              [ Output
                  { outputAddress = optionsAddress
                  , outputValue = utxoValue (fromIntegral $ contractDeposit + 1_000_000) $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                      , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                      ]
                  , outputDatum = OutputDatum $ toDatum datum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = toCardanoApiAddress paymentAddress
                  , outputValue = utxoValue 3_000_000 $ mconcat
                      [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                  , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-- | Purchase a single valid Proposal UTxO. The Proposal UTxO has only one possible `Terms`.
-- The contract output is first. The offer asset is ada. More ada is deposited.
regressionTest18 :: MonadEmulator m => m ()
regressionTest18 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

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
  mintTestTokens writerWallet 10_000_000 [("TestToken2",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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

  proposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let desiredTerms = zip proposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap proposals $ \(_,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unPremiumBeacon premiumBeacon, -1)
                  ]
              , mintRedeemer = toRedeemer BurnProposalBeacons
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap proposals $ \(ref,Just ProposalDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, 1)
                  , (unAskBeacon askBeacon, 1)
                  , (unTradingPairBeacon tradingPairBeacon, 1)
                  , (unContractId $ genContractId ref, 2)
                  ]
              , mintRedeemer = 
                  toRedeemer $ PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = flip map desiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap desiredTerms $ 
          \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
            let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
                Terms{premium} = possibleTerms !! idx
            in
              [ Output
                  { outputAddress = optionsAddress
                  , outputValue = utxoValue (fromIntegral $ contractDeposit + 1_000_000) $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                      , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                      ]
                  , outputDatum = OutputDatum $ toDatum datum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = toCardanoApiAddress paymentAddress
                  , outputValue = utxoValue 3_000_000 $ mconcat
                      [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                  , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all regression scenarios for purchasing Proposal UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "regressionTest1" regressionTest1
  , mustSucceed "regressionTest2" regressionTest2
  , mustSucceed "regressionTest3" regressionTest3
  , mustSucceed "regressionTest4" regressionTest4
  , mustSucceed "regressionTest5" regressionTest5
  , mustSucceed "regressionTest6" regressionTest6
  , mustSucceed "regressionTest7" regressionTest7
  , mustSucceed "regressionTest8" regressionTest8
  , mustSucceed "regressionTest9" regressionTest9
  , mustSucceed "regressionTest10" regressionTest10
  , mustSucceed "regressionTest11" regressionTest11
  , mustSucceed "regressionTest12" regressionTest12
  , mustSucceed "regressionTest13" regressionTest13
  , mustSucceed "regressionTest14" regressionTest14
  , mustSucceed "regressionTest15" regressionTest15
  , mustSucceed "regressionTest16" regressionTest16
  , mustSucceed "regressionTest17" regressionTest17
  , mustSucceed "regressionTest18" regressionTest18
  ]
