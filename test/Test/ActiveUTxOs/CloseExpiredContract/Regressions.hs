{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.ActiveUTxOs.CloseExpiredContract.Regressions where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Data.List (sortOn)

import CardanoOptions

import Test.Prelude

-------------------------------------------------
-- Basic Regression Tests
-------------------------------------------------
-- | Close a single expired contract.
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

  claimSlot <- awaitTime (slotToPosixTime 1000) >> currentSlot

  actives <- txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Try to close the contract.
  void $ transact writerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unContractId contractId, -1)
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
                    SpendWithPlutusReference optionsRef InlineDatum $ 
                      toRedeemer CloseExpiredContract
                }
          ]
      , referenceInputs = [optionsRef,activeBeaconsRef]
      , extraKeyWitnesses = [writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Close multiple expired contracts. All contracts use the same assets and beacons. The contracts
-- come from the same writer address.
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

  claimSlot <- awaitTime (slotToPosixTime 1000) >> currentSlot

  actives <- txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Try to close the contract.
  void $ transact writerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unContractId contractId, -1)
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
                    SpendWithPlutusReference optionsRef InlineDatum $ 
                      toRedeemer CloseExpiredContract
                }
          ]
      , referenceInputs = [optionsRef,activeBeaconsRef]
      , extraKeyWitnesses = [writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Close multiple expired contracts. All contracts use different assets and beacons. The contracts
-- come from the same writer address.
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
                , strikePrice = Fraction (1,1)
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
                , strikePrice = Fraction (1,1)
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

  claimSlot <- awaitTime (slotToPosixTime 1000) >> currentSlot

  actives <- txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Try to close the contract.
  void $ transact writerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unContractId contractId, -1)
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
                    SpendWithPlutusReference optionsRef InlineDatum $ 
                      toRedeemer CloseExpiredContract
                }
          ]
      , referenceInputs = [optionsRef,activeBeaconsRef]
      , extraKeyWitnesses = [writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Close multiple expired contracts that are from different writer addresses. The required
-- credentials approve the transaction.
regressionTest4 :: MonadEmulator m => m ()
regressionTest4 = do
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
      writerPayPrivKey2 = Mock.paymentPrivateKey writerWallet2
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

  claimSlot <- awaitTime (slotToPosixTime 1000) >> currentSlot

  actives <- fmap (sortOn fst) $
    (<>) <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress1
         <*> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress2
  
  -- Try to close the contract.
  void $ transact 
    writerPersonalAddr1 
    [refScriptAddress,optionsAddress1,optionsAddress2] 
    [writerPayPrivKey1,writerPayPrivKey2]
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unContractId contractId, -1)
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
                    SpendWithPlutusReference optionsRef InlineDatum $ 
                      toRedeemer CloseExpiredContract
                }
          ]
      , referenceInputs = [optionsRef,activeBeaconsRef]
      , extraKeyWitnesses = [writerPubKey1,writerPubKey2]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Close an invalid Active UTxO.
regressionTest5 :: MonadEmulator m => m ()
regressionTest5 = do
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

      invalidDatum = createActiveDatumFromProposal 0 (TxOutRef (TxId "") 0) proposalDatum 

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens writerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
      , outputs = mconcat
          [ flip concatMap desiredTerms $ 
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
          , [ Output
                { outputAddress = optionsAddress
                , outputValue = utxoValue 3_000_000 $ mempty
                , outputDatum = OutputDatum $ toDatum invalidDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      }

  claimSlot <- awaitTime (slotToPosixTime 1000) >> currentSlot

  actives <- txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Try to close the contract.
  void $ transact writerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  if contractId == genContractId (TxOutRef (TxId "") 0) then [] else
                    [ (unOfferBeacon offerBeacon, -1)
                    , (unAskBeacon askBeacon, -1)
                    , (unTradingPairBeacon tradingPairBeacon, -1)
                    , (unContractId contractId, -1)
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
                    SpendWithPlutusReference optionsRef InlineDatum $ 
                      toRedeemer CloseExpiredContract
                }
          ]
      , referenceInputs = [optionsRef,activeBeaconsRef]
      , extraKeyWitnesses = [writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Close a single expired contract in the same transaction where a Proposal UTxO is created.
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
      newProposalDatum = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (adaSymbol,adaToken)
        , offerQuantity = 10_000_000
        , askAsset = AskAsset (testTokenSymbol,"TestToken1")
        , premiumAsset = PremiumAsset (adaSymbol,adaToken)
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 3000
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

  claimSlot <- awaitTime (slotToPosixTime 1000) >> currentSlot

  actives <- txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Try to close the contract.
  void $ transact writerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  [ (unOfferBeacon offerBeacon, -1)
                  , (unAskBeacon askBeacon, -1)
                  , (unTradingPairBeacon tradingPairBeacon, -1)
                  , (unContractId contractId, -1)
                  ]
              , mintRedeemer = toRedeemer $ 
                  PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          , TokenMint
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
      , inputs = mconcat
          [ flip map actives $ \(ref,_) ->
              Input
                { inputId = ref
                , inputWitness = 
                    SpendWithPlutusReference optionsRef InlineDatum $ 
                      toRedeemer CloseExpiredContract
                }
          ]
      , outputs = flip map [newProposalDatum] $ \datum@ProposalDatum{..} ->
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
      , referenceInputs = [optionsRef,activeBeaconsRef,proposalBeaconsRef]
      , extraKeyWitnesses = [writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Just 3000
          }
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all regression scenarios for closing expired contracts.
tests :: [TestTree]
tests =
  [ mustSucceed "regressionTest1" regressionTest1
  , mustSucceed "regressionTest2" regressionTest2
  , mustSucceed "regressionTest3" regressionTest3
  , mustSucceed "regressionTest4" regressionTest4
  , mustSucceed "regressionTest5" regressionTest5
  , mustSucceed "regressionTest6" regressionTest6
  ]
