{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.ActiveUTxOs.ExecuteContract.Benchmarks where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Control.Monad (forM_,replicateM_)
import Data.String (fromString)

import CardanoOptions

import Test.Prelude

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Execute multiple Active UTxOs. All Active UTxOs are for the same terms and have the same 
-- beacons. The key NFTs are in separate UTxOs.
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 number = do
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
      buyerWallet = Mock.knownMockWallet 1
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
  replicateM_ 3 $ transact writerPersonalAddr [refScriptAddress] [writerPayPrivKey]
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [proposalDatum] $ \ProposalDatum{..} ->
                  [ (unOfferBeacon offerBeacon, 10)
                  , (unAskBeacon askBeacon, 10)
                  , (unTradingPairBeacon tradingPairBeacon, 10)
                  , (unPremiumBeacon premiumBeacon, 10)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateProposals
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          ]
      , outputs = flip map (replicate 10 proposalDatum) $ \datum@ProposalDatum{..} ->
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

  allProposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress

  -- Try to buy the Proposal UTxO.
  forM_ (grouped 5 allProposals) $ \proposals -> do
    let desiredTerms = zip proposals $ repeat (0 :: Int)
    transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey]
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
                        [ PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                        , uncurry PV2.singleton (unPremiumAsset premiumAsset) premium
                        ]
                    , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                ]
        , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
        , extraKeyWitnesses = [buyerPubKey]
        }

  actives <- take number <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Create a clean UTxO to use.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { outputs =
          [ Output
              { outputAddress = buyerPersonalAddr
              , outputValue = utxoValue 100_000_000 mempty
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = buyerPersonalAddr
              , outputValue = utxoValue 100_000_000 mempty
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      }

  -- Try to execute the contract.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unContractId contractId, -2)
                  ]
              , mintRedeemer = toRedeemer $ 
                  PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = mconcat
          [ flip map actives $ \(ref,_) ->
              Input
                { inputId = ref
                , inputWitness = 
                    SpendWithPlutusReference optionsRef InlineDatum $ toRedeemer ExecuteContract
                }
          ]
      , outputs = flip map actives $ \(_,Just ActiveDatum{..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                    requiredAmount offerQuantity strikePrice
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [optionsRef,activeBeaconsRef]
      , extraKeyWitnesses = [buyerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 1000
          }
      }

-- | Execute multiple Active UTxOs. All Active UTxOs are for different terms and have different 
-- beacons.
benchTest2 :: MonadEmulator m => Int -> m ()
benchTest2 number = do
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
      buyerWallet = Mock.knownMockWallet 1
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..90]
      
      -- Contract Info
      proposalDatums = flip map (grouped 3 assetNames) $ \[asset1,asset2,asset3] -> 
        unsafeCreateProposalDatum $ NewProposalInfo
          { offerAsset = OfferAsset (testTokenSymbol,asset1)
          , offerQuantity = 10
          , askAsset = AskAsset (testTokenSymbol,asset2)
          , premiumAsset = PremiumAsset (testTokenSymbol,asset3)
          , contractDeposit = 5_000_000
          , paymentAddress = toPlutusAddress writerPersonalAddr
          , possibleTerms =
              [ Terms
                  { expiration = slotToPosixTime 1000
                  , strikePrice = Fraction (1,1)
                  , premium = 2
                  }
              ]
          }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 $ zip assetNames $ repeat 1000
  mintTestTokens buyerWallet 10_000_000 $ zip assetNames $ repeat 1000

  -- Try to create the Proposal UTxO.
  forM_ (grouped 10 proposalDatums) $ \proposals -> do
    transact writerPersonalAddr [refScriptAddress] [writerPayPrivKey]
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = flip concatMap proposals $ \ProposalDatum{..} ->
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
        , outputs = flip map proposals $ \datum@ProposalDatum{..} ->
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

  allProposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress

  -- Try to buy the Proposal UTxO.
  forM_ (grouped 2 allProposals) $ \proposals -> do
    let desiredTerms = zip proposals $ repeat (0 :: Int)
    transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey]
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
                        [ PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                        , uncurry PV2.singleton (unPremiumAsset premiumAsset) premium
                        ]
                    , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                ]
        , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
        , extraKeyWitnesses = [buyerPubKey]
        }

  actives <- take number <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Create a clean UTxO to use.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { outputs =
          [ Output
              { outputAddress = buyerPersonalAddr
              , outputValue = utxoValue 100_000_000 mempty
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = buyerPersonalAddr
              , outputValue = utxoValue 100_000_000 mempty
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      }

  -- Try to execute the contract.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unContractId contractId, -2)
                  ]
              , mintRedeemer = toRedeemer $ 
                  PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = mconcat
          [ flip map actives $ \(ref,_) ->
              Input
                { inputId = ref
                , inputWitness = 
                    SpendWithPlutusReference optionsRef InlineDatum $ toRedeemer ExecuteContract
                }
          ]
      , outputs = flip map actives $ \(_,Just ActiveDatum{..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                    requiredAmount offerQuantity strikePrice
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [optionsRef,activeBeaconsRef]
      , extraKeyWitnesses = [buyerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 1000
          }
      }

-- | Execute multiple Active UTxOs. All Active UTxOs are for the same terms and have the same 
-- beacons. The key NFTs are in the same UTxO.
benchTest3 :: MonadEmulator m => Int -> m ()
benchTest3 number = do
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
      buyerWallet = Mock.knownMockWallet 1
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
  replicateM_ 3 $ transact writerPersonalAddr [refScriptAddress] [writerPayPrivKey]
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [proposalDatum] $ \ProposalDatum{..} ->
                  [ (unOfferBeacon offerBeacon, 10)
                  , (unAskBeacon askBeacon, 10)
                  , (unTradingPairBeacon tradingPairBeacon, 10)
                  , (unPremiumBeacon premiumBeacon, 10)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateProposals
              , mintPolicy = toVersionedMintingPolicy proposalBeaconScript
              , mintReference = Just proposalBeaconsRef
              }
          ]
      , outputs = flip map (replicate 10 proposalDatum) $ \datum@ProposalDatum{..} ->
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

  allProposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress

  -- Try to buy the Proposal UTxO.
  forM_ (grouped 5 allProposals) $ \proposals -> do
    let desiredTerms = zip proposals $ repeat (0 :: Int)
    transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey]
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
                        [ PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                        , uncurry PV2.singleton (unPremiumAsset premiumAsset) premium
                        ]
                    , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                ]
        , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
        , extraKeyWitnesses = [buyerPubKey]
        }

  actives <- take number <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Create a clean UTxO to use.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { outputs =
          [ Output
              { outputAddress = buyerPersonalAddr
              , outputValue = utxoValue 100_000_000 mempty
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = buyerPersonalAddr
              , outputValue = utxoValue 100_000_000 $ mconcat $ flip map actives $
                  \(_,Just ActiveDatum{contractId}) -> 
                    PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      }

  -- Try to execute the contract.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unContractId contractId, -2)
                  ]
              , mintRedeemer = toRedeemer $ 
                  PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = mconcat
          [ flip map actives $ \(ref,_) ->
              Input
                { inputId = ref
                , inputWitness = 
                    SpendWithPlutusReference optionsRef InlineDatum $ toRedeemer ExecuteContract
                }
          ]
      , outputs = flip map actives $ \(_,Just ActiveDatum{..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                    requiredAmount offerQuantity strikePrice
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [optionsRef,activeBeaconsRef]
      , extraKeyWitnesses = [buyerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 1000
          }
      }

-- | Execute multiple Active UTxOs. All Active UTxOs are for different terms and have different 
-- beacons.
benchTest4 :: MonadEmulator m => Int -> m ()
benchTest4 number = do
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
      buyerWallet = Mock.knownMockWallet 1
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..90]
      
      -- Contract Info
      proposalDatums = flip map (grouped 3 assetNames) $ \[asset1,asset2,asset3] -> 
        unsafeCreateProposalDatum $ NewProposalInfo
          { offerAsset = OfferAsset (testTokenSymbol,asset1)
          , offerQuantity = 10
          , askAsset = AskAsset (testTokenSymbol,asset2)
          , premiumAsset = PremiumAsset (testTokenSymbol,asset3)
          , contractDeposit = 5_000_000
          , paymentAddress = toPlutusAddress writerPersonalAddr
          , possibleTerms =
              [ Terms
                  { expiration = slotToPosixTime 1000
                  , strikePrice = Fraction (1,1)
                  , premium = 2
                  }
              ]
          }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 $ zip assetNames $ repeat 1000
  mintTestTokens buyerWallet 10_000_000 $ zip assetNames $ repeat 1000

  -- Try to create the Proposal UTxO.
  forM_ (grouped 10 proposalDatums) $ \proposals -> do
    transact writerPersonalAddr [refScriptAddress] [writerPayPrivKey]
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = flip concatMap proposals $ \ProposalDatum{..} ->
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
        , outputs = flip map proposals $ \datum@ProposalDatum{..} ->
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

  allProposals <- txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress

  -- Try to buy the Proposal UTxO.
  forM_ (grouped 2 allProposals) $ \proposals -> do
    let desiredTerms = zip proposals $ repeat (0 :: Int)
    transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey]
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
                        [ PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                        , uncurry PV2.singleton (unPremiumAsset premiumAsset) premium
                        ]
                    , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                ]
        , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
        , extraKeyWitnesses = [buyerPubKey]
        }

  actives <- take number <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Create a clean UTxO to use.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { outputs =
          [ Output
              { outputAddress = buyerPersonalAddr
              , outputValue = utxoValue 100_000_000 mempty
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = buyerPersonalAddr
              , outputValue = utxoValue 100_000_000 $ mconcat $ flip map actives $
                  \(_,Just ActiveDatum{contractId}) -> 
                    PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      }

  -- Try to execute the contract.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unContractId contractId, -2)
                  ]
              , mintRedeemer = toRedeemer $ 
                  PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = mconcat
          [ flip map actives $ \(ref,_) ->
              Input
                { inputId = ref
                , inputWitness = 
                    SpendWithPlutusReference optionsRef InlineDatum $ toRedeemer ExecuteContract
                }
          ]
      , outputs = flip map actives $ \(_,Just ActiveDatum{..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                    requiredAmount offerQuantity strikePrice
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [optionsRef,activeBeaconsRef]
      , extraKeyWitnesses = [buyerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 1000
          }
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for executing contracts.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 18
  , mustSucceed "benchTest2" $ benchTest2 12
  , mustSucceed "benchTest3" $ benchTest3 18
  , mustSucceed "benchTest4" $ benchTest4 12

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 19
  , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 13
  , mustExceedTxLimits "perfIncreaseTest3" $ benchTest3 19
  , mustExceedTxLimits "perfIncreaseTest4" $ benchTest4 13
  ]
