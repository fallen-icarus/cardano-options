{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.ActiveUTxOs.Misc where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (testGroup,TestTree)
import Data.Maybe (isJust)
import Optics.Operators
import Optics.Optic

import CardanoOptions

import Test.Prelude

-------------------------------------------------
-- Miscelleneous Regression Tests
-------------------------------------------------
-- | Execute a contract in the same transaction where an expired contract is closed.
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
                { expiration = slotToPosixTime 3000
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

  claimSlot <- awaitTime (slotToPosixTime 2000) >> currentSlot

  actives <- txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Try to execute the contract.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey,buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  if expiration == slotToPosixTime 1000 then
                    [ (unOfferBeacon offerBeacon, -1)
                    , (unAskBeacon askBeacon, -1)
                    , (unTradingPairBeacon tradingPairBeacon, -1)
                    , (unContractId contractId, -1)
                    ]
                  else
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
          [ flip map actives $ \(ref,Just ActiveDatum{expiration}) ->
              if expiration == slotToPosixTime 1000 then
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer CloseExpiredContract
                  }
              else
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ toRedeemer ExecuteContract
                  }
          ]
      , outputs = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
          if expiration == slotToPosixTime 1000 then [] else
            [ Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                    [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                        requiredAmount offerQuantity strikePrice 
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
      , referenceInputs = [optionsRef,activeBeaconsRef]
      , extraKeyWitnesses = [buyerPubKey,writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Just 3000
          }
      }

-- | Execute a contract in the same transaction where a Proposal UTxO is purchased.
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
                { expiration = slotToPosixTime 3000
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

  initialProposals <- take 1 <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let initialDesiredTerms = zip initialProposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap initialProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap initialProposals $ \(ref,Just ProposalDatum{..}) ->
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
      , inputs = flip map initialDesiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap initialDesiredTerms $ 
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

  remProposals <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let remDesiredTerms = zip remProposals $ repeat (0 :: Int)

  actives <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap remProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap remProposals $ \(ref,Just ProposalDatum{..}) ->
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
          [ flip map remDesiredTerms $ \((ref,_),idx) ->
              Input
                { inputId = ref
                , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                    toRedeemer $ PurchaseContract $ fromIntegral idx
                }
          , flip map actives $ \(ref,Just ActiveDatum{}) ->
              Input
                { inputId = ref
                , inputWitness = 
                    SpendWithPlutusReference optionsRef InlineDatum $ toRedeemer ExecuteContract
                }
          ]
      , outputs = mconcat
          [ flip concatMap remDesiredTerms $ 
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
          , flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
              [ Output
                  { outputAddress = toCardanoApiAddress paymentAddress
                  , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                      [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                          requiredAmount offerQuantity strikePrice 
                      ]
                  , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
          ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 1000
          }
      }

-- | Close an expired contract in the same transaction where a Proposal UTxO is purchased.
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
                { expiration = slotToPosixTime 3000
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

  initialProposals <- take 1 <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let initialDesiredTerms = zip initialProposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap initialProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap initialProposals $ \(ref,Just ProposalDatum{..}) ->
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
      , inputs = flip map initialDesiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap initialDesiredTerms $ 
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

  remProposals <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let remDesiredTerms = zip remProposals $ repeat (0 :: Int)

  claimSlot <- awaitTime (slotToPosixTime 1000) >> currentSlot

  actives <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey,buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap remProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap remProposals $ \(ref,Just ProposalDatum{..}) ->
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
          [ flip map remDesiredTerms $ \((ref,_),idx) ->
              Input
                { inputId = ref
                , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                    toRedeemer $ PurchaseContract $ fromIntegral idx
                }
          , flip map actives $ \(ref,Just ActiveDatum{}) ->
              Input
                { inputId = ref
                , inputWitness = 
                    SpendWithPlutusReference optionsRef InlineDatum $ 
                      toRedeemer CloseExpiredContract
                }
          ]
      , outputs = mconcat
          [ flip concatMap remDesiredTerms $ 
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
          ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [writerPubKey,buyerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Execute a contract, close an expired contract, and purchase a Proposal UTxO all in the same
-- transaction.
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
                { expiration = slotToPosixTime 2000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }
      proposalDatum3 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 3000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }

      proposalDatums = [proposalDatum1, proposalDatum2, proposalDatum3]

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
              { mintTokens = flip concatMap proposalDatums $ 
                  \ProposalDatum{..} ->
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
      , outputs = flip map proposalDatums $ \datum@ProposalDatum{..} ->
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

  initialProposals <- take 2 <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let initialDesiredTerms = zip initialProposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap initialProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap initialProposals $ \(ref,Just ProposalDatum{..}) ->
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
      , inputs = flip map initialDesiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap initialDesiredTerms $ 
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

  claimSlot <- awaitTime (slotToPosixTime 1500) >> currentSlot

  remProposals <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let remDesiredTerms = zip remProposals $ repeat (0 :: Int)

  actives <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Try the composition.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey,buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap remProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap remProposals $ \(ref,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  if expiration == slotToPosixTime 1000 then
                    [ (unOfferBeacon offerBeacon, -1)
                    , (unAskBeacon askBeacon, -1)
                    , (unTradingPairBeacon tradingPairBeacon, -1)
                    , (unContractId contractId, -1)
                    ]
                  else
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
          [ flip map remDesiredTerms $ \((ref,_),idx) ->
              Input
                { inputId = ref
                , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                    toRedeemer $ PurchaseContract $ fromIntegral idx
                }
          , flip map actives $ \(ref,Just ActiveDatum{expiration}) ->
              if expiration == slotToPosixTime 1000 then
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer CloseExpiredContract
                  }
              else
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer ExecuteContract
                  }
          ]
      , outputs = mconcat
          [ flip concatMap remDesiredTerms $ 
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
          , flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
              if expiration == slotToPosixTime 1000 then [] else
                [ Output
                    { outputAddress = toCardanoApiAddress paymentAddress
                    , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                        [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                            requiredAmount offerQuantity strikePrice 
                        ]
                    , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                ]
          ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey,writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Just 2000
          }
      }

-- | Execute a contract, close an expired contract, purchase a Proposal UTxO, and update a payment
-- address all in the same transaction.
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

      newPaymentAddr = PV2.Address 
        { addressCredential = PV2.ScriptCredential $ scriptHash proxyScript
        , addressStakingCredential = Just $ PV2.StakingHash writerCred
        }

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
                { expiration = slotToPosixTime 2000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }
      proposalDatum3 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 3000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }
      proposalDatum4 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 4000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }

      proposalDatums = [proposalDatum1, proposalDatum2, proposalDatum3, proposalDatum4]

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
              { mintTokens = flip concatMap proposalDatums $ 
                  \ProposalDatum{..} ->
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
      , outputs = flip map proposalDatums $ \datum@ProposalDatum{..} ->
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

  initialProposals <- take 3 <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let initialDesiredTerms = zip initialProposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap initialProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap initialProposals $ \(ref,Just ProposalDatum{..}) ->
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
      , inputs = flip map initialDesiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap initialDesiredTerms $ 
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

  claimSlot <- awaitTime (slotToPosixTime 1500) >> currentSlot

  remProposals <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let remDesiredTerms = zip remProposals $ repeat (0 :: Int)

  actives <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Try the composition.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey,buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap remProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap remProposals $ \(ref,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  if expiration == slotToPosixTime 1000 then
                    [ (unOfferBeacon offerBeacon, -1)
                    , (unAskBeacon askBeacon, -1)
                    , (unTradingPairBeacon tradingPairBeacon, -1)
                    , (unContractId contractId, -1)
                    ]
                  else if expiration == slotToPosixTime 2000 then
                    [ (unOfferBeacon offerBeacon, -1)
                    , (unAskBeacon askBeacon, -1)
                    , (unTradingPairBeacon tradingPairBeacon, -1)
                    , (unContractId contractId, -2)
                    ]
                  else []
              , mintRedeemer = toRedeemer $ 
                  PurchaseExecuteOrCloseExpiredContracts proposalBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , inputs = mconcat
          [ flip map remDesiredTerms $ \((ref,_),idx) ->
              Input
                { inputId = ref
                , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                    toRedeemer $ PurchaseContract $ fromIntegral idx
                }
          , flip map actives $ \(ref,Just ActiveDatum{expiration}) ->
              if expiration == slotToPosixTime 1000 then
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer CloseExpiredContract
                  }
              else if expiration == slotToPosixTime 2000 then
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer ExecuteContract
                  }
              else
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer $ UpdatePaymentAddress newPaymentAddr 0
                  }
          ]
      , outputs = mconcat
          [ flip concatMap remDesiredTerms $ 
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
          , flip concatMap actives $ \(_,Just datum@ActiveDatum{..}) ->
              if expiration == slotToPosixTime 1000 then [] 
              else if expiration == slotToPosixTime 2000 then
                [ Output
                    { outputAddress = toCardanoApiAddress paymentAddress
                    , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                        [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                            requiredAmount offerQuantity strikePrice 
                        ]
                    , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                ]
              else
                [ Output
                    { outputAddress = optionsAddress
                    , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                        [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                        , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                        , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                        , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 1
                        , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                        ]
                    , outputDatum = OutputDatum $ toDatum $ 
                        datum & #paymentAddress .~ newPaymentAddr
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                ]
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressObserverRef,proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey,writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Just 2000
          }
      }

-------------------------------------------------
-- Miscelleneous Failure Tests
-------------------------------------------------
-- | When executing a contract, closing an expired contract, and purchasing a Proposal UTxO all in 
-- the same transaction; there is an unrelated output between all of the required outputs. The
-- required outputs are: contracts, then premium, then execution. The first contract output is
-- invalid.
failureTest1 :: MonadEmulator m => m ()
failureTest1 = do
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
                { expiration = slotToPosixTime 2000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }
      proposalDatum3 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 3000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }

      proposalDatums = [proposalDatum1, proposalDatum2, proposalDatum3]

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
              { mintTokens = flip concatMap proposalDatums $ 
                  \ProposalDatum{..} ->
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
      , outputs = flip map proposalDatums $ \datum@ProposalDatum{..} ->
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

  initialProposals <- take 2 <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let initialDesiredTerms = zip initialProposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap initialProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap initialProposals $ \(ref,Just ProposalDatum{..}) ->
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
      , inputs = flip map initialDesiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap initialDesiredTerms $ 
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

  claimSlot <- awaitTime (slotToPosixTime 1500) >> currentSlot

  remProposals <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let remDesiredTerms = zip remProposals $ repeat (0 :: Int)

  actives <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  let contractOutputs = flip map remDesiredTerms $ 
        \((ref,Just pd),idx) ->
          let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
          in Output
                { outputAddress = optionsAddress
                , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 0
                    , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }

      premiumOutputs = flip map remDesiredTerms $
        \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
          let ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
              Terms{premium} = possibleTerms !! idx
          in Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }

      executionOutputs = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
        if expiration == slotToPosixTime 1000 then [] else
          [ Output
              { outputAddress = toCardanoApiAddress paymentAddress
              , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                  [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                      requiredAmount offerQuantity strikePrice 
                  ]
              , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

      extraOutput =
        [ Output
            { outputAddress = buyerPersonalAddr
            , outputValue = utxoValue 5_000_000 mempty
            , outputDatum = NoOutputDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
        ]

  -- Try the composition.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey,buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap remProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap remProposals $ \(ref,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  if expiration == slotToPosixTime 1000 then
                    [ (unOfferBeacon offerBeacon, -1)
                    , (unAskBeacon askBeacon, -1)
                    , (unTradingPairBeacon tradingPairBeacon, -1)
                    , (unContractId contractId, -1)
                    ]
                  else
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
          [ flip map remDesiredTerms $ \((ref,_),idx) ->
              Input
                { inputId = ref
                , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                    toRedeemer $ PurchaseContract $ fromIntegral idx
                }
          , flip map actives $ \(ref,Just ActiveDatum{expiration}) ->
              if expiration == slotToPosixTime 1000 then
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer CloseExpiredContract
                  }
              else
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer ExecuteContract
                  }
          ]
      , outputs = mconcat
          [ extraOutput
          , contractOutputs
          , extraOutput
          , premiumOutputs
          , extraOutput
          , executionOutputs
          ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey,writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Just 2000
          }
      }

-- | When executing a contract, closing an expired contract, and purchasing a Proposal UTxO all in 
-- the same transaction; there is an unrelated output between all of the required outputs. The
-- required outputs are: contracts, then premium, then execution. The first premium output is
-- invalid.
failureTest2 :: MonadEmulator m => m ()
failureTest2 = do
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
                { expiration = slotToPosixTime 2000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }
      proposalDatum3 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 3000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }

      proposalDatums = [proposalDatum1, proposalDatum2, proposalDatum3]

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
              { mintTokens = flip concatMap proposalDatums $ 
                  \ProposalDatum{..} ->
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
      , outputs = flip map proposalDatums $ \datum@ProposalDatum{..} ->
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

  initialProposals <- take 2 <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let initialDesiredTerms = zip initialProposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap initialProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap initialProposals $ \(ref,Just ProposalDatum{..}) ->
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
      , inputs = flip map initialDesiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap initialDesiredTerms $ 
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

  claimSlot <- awaitTime (slotToPosixTime 1500) >> currentSlot

  remProposals <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let remDesiredTerms = zip remProposals $ repeat (0 :: Int)

  actives <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  let contractOutputs = flip map remDesiredTerms $ 
        \((ref,Just pd),idx) ->
          let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
          in Output
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

      premiumOutputs = flip map remDesiredTerms $
        \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
          let ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
              Terms{premium} = possibleTerms !! idx
          in Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ uncurry PV2.singleton (unPremiumAsset premiumAsset) $ premium - 1 ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }

      executionOutputs = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
        if expiration == slotToPosixTime 1000 then [] else
          [ Output
              { outputAddress = toCardanoApiAddress paymentAddress
              , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                  [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                      requiredAmount offerQuantity strikePrice 
                  ]
              , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

      extraOutput =
        [ Output
            { outputAddress = buyerPersonalAddr
            , outputValue = utxoValue 5_000_000 mempty
            , outputDatum = NoOutputDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
        ]

  -- Try the composition.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey,buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap remProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap remProposals $ \(ref,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  if expiration == slotToPosixTime 1000 then
                    [ (unOfferBeacon offerBeacon, -1)
                    , (unAskBeacon askBeacon, -1)
                    , (unTradingPairBeacon tradingPairBeacon, -1)
                    , (unContractId contractId, -1)
                    ]
                  else
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
          [ flip map remDesiredTerms $ \((ref,_),idx) ->
              Input
                { inputId = ref
                , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                    toRedeemer $ PurchaseContract $ fromIntegral idx
                }
          , flip map actives $ \(ref,Just ActiveDatum{expiration}) ->
              if expiration == slotToPosixTime 1000 then
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer CloseExpiredContract
                  }
              else
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer ExecuteContract
                  }
          ]
      , outputs = mconcat
          [ extraOutput
          , contractOutputs
          , extraOutput
          , premiumOutputs
          , extraOutput
          , executionOutputs
          ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey,writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Just 2000
          }
      }

-- | When executing a contract, closing an expired contract, and purchasing a Proposal UTxO all in 
-- the same transaction; there is an unrelated output between all of the required outputs. The
-- required outputs are: contracts, then premium, then execution. The first execution output is
-- invalid.
failureTest3 :: MonadEmulator m => m ()
failureTest3 = do
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
                { expiration = slotToPosixTime 2000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }
      proposalDatum3 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 3000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }

      proposalDatums = [proposalDatum1, proposalDatum2, proposalDatum3]

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
              { mintTokens = flip concatMap proposalDatums $ 
                  \ProposalDatum{..} ->
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
      , outputs = flip map proposalDatums $ \datum@ProposalDatum{..} ->
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

  initialProposals <- take 2 <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let initialDesiredTerms = zip initialProposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap initialProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap initialProposals $ \(ref,Just ProposalDatum{..}) ->
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
      , inputs = flip map initialDesiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap initialDesiredTerms $ 
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

  claimSlot <- awaitTime (slotToPosixTime 1500) >> currentSlot

  remProposals <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let remDesiredTerms = zip remProposals $ repeat (0 :: Int)

  actives <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  let contractOutputs = flip map remDesiredTerms $ 
        \((ref,Just pd),idx) ->
          let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
          in Output
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

      premiumOutputs = flip map remDesiredTerms $
        \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
          let ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
              Terms{premium} = possibleTerms !! idx
          in Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }

      executionOutputs = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
        if expiration == slotToPosixTime 1000 then [] else
          [ Output
              { outputAddress = toCardanoApiAddress paymentAddress
              , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                  [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                      requiredAmount offerQuantity strikePrice - 1
                  ]
              , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

      extraOutput =
        [ Output
            { outputAddress = buyerPersonalAddr
            , outputValue = utxoValue 5_000_000 mempty
            , outputDatum = NoOutputDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
        ]

  -- Try the composition.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey,buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap remProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap remProposals $ \(ref,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  if expiration == slotToPosixTime 1000 then
                    [ (unOfferBeacon offerBeacon, -1)
                    , (unAskBeacon askBeacon, -1)
                    , (unTradingPairBeacon tradingPairBeacon, -1)
                    , (unContractId contractId, -1)
                    ]
                  else
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
          [ flip map remDesiredTerms $ \((ref,_),idx) ->
              Input
                { inputId = ref
                , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                    toRedeemer $ PurchaseContract $ fromIntegral idx
                }
          , flip map actives $ \(ref,Just ActiveDatum{expiration}) ->
              if expiration == slotToPosixTime 1000 then
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer CloseExpiredContract
                  }
              else
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer ExecuteContract
                  }
          ]
      , outputs = mconcat
          [ extraOutput
          , contractOutputs
          , extraOutput
          , premiumOutputs
          , extraOutput
          , executionOutputs
          ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey,writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Just 2000
          }
      }

-- | When executing a contract, closing an expired contract, and purchasing a Proposal UTxO all in 
-- the same transaction; there is an unrelated output between all of the required outputs. The
-- required outputs are: premium, then contracts, then execution. The first contract output is
-- invalid.
failureTest4 :: MonadEmulator m => m ()
failureTest4 = do
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
                { expiration = slotToPosixTime 2000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }
      proposalDatum3 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 3000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }

      proposalDatums = [proposalDatum1, proposalDatum2, proposalDatum3]

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
              { mintTokens = flip concatMap proposalDatums $ 
                  \ProposalDatum{..} ->
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
      , outputs = flip map proposalDatums $ \datum@ProposalDatum{..} ->
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

  initialProposals <- take 2 <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let initialDesiredTerms = zip initialProposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap initialProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap initialProposals $ \(ref,Just ProposalDatum{..}) ->
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
      , inputs = flip map initialDesiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap initialDesiredTerms $ 
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

  claimSlot <- awaitTime (slotToPosixTime 1500) >> currentSlot

  remProposals <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let remDesiredTerms = zip remProposals $ repeat (0 :: Int)

  actives <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  let contractOutputs = flip map remDesiredTerms $ 
        \((ref,Just pd),idx) ->
          let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
          in Output
                { outputAddress = optionsAddress
                , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (unOfferBeacon offerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unAskBeacon askBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unTradingPairBeacon tradingPairBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (unContractId contractId) 0
                    , uncurry PV2.singleton (unOfferAsset offerAsset) offerQuantity
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }

      premiumOutputs = flip map remDesiredTerms $
        \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
          let ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
              Terms{premium} = possibleTerms !! idx
          in Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }

      executionOutputs = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
        if expiration == slotToPosixTime 1000 then [] else
          [ Output
              { outputAddress = toCardanoApiAddress paymentAddress
              , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                  [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                      requiredAmount offerQuantity strikePrice 
                  ]
              , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

      extraOutput =
        [ Output
            { outputAddress = buyerPersonalAddr
            , outputValue = utxoValue 5_000_000 mempty
            , outputDatum = NoOutputDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
        ]

  -- Try the composition.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey,buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap remProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap remProposals $ \(ref,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  if expiration == slotToPosixTime 1000 then
                    [ (unOfferBeacon offerBeacon, -1)
                    , (unAskBeacon askBeacon, -1)
                    , (unTradingPairBeacon tradingPairBeacon, -1)
                    , (unContractId contractId, -1)
                    ]
                  else
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
          [ flip map remDesiredTerms $ \((ref,_),idx) ->
              Input
                { inputId = ref
                , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                    toRedeemer $ PurchaseContract $ fromIntegral idx
                }
          , flip map actives $ \(ref,Just ActiveDatum{expiration}) ->
              if expiration == slotToPosixTime 1000 then
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer CloseExpiredContract
                  }
              else
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer ExecuteContract
                  }
          ]
      , outputs = mconcat
          [ extraOutput
          , premiumOutputs
          , extraOutput
          , contractOutputs
          , extraOutput
          , executionOutputs
          ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey,writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Just 2000
          }
      }

-- | When executing a contract, closing an expired contract, and purchasing a Proposal UTxO all in 
-- the same transaction; there is an unrelated output between all of the required outputs. The
-- required outputs are: premium, then contracts, then execution. The first premium output is
-- invalid.
failureTest5 :: MonadEmulator m => m ()
failureTest5 = do
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
                { expiration = slotToPosixTime 2000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }
      proposalDatum3 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 3000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }

      proposalDatums = [proposalDatum1, proposalDatum2, proposalDatum3]

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
              { mintTokens = flip concatMap proposalDatums $ 
                  \ProposalDatum{..} ->
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
      , outputs = flip map proposalDatums $ \datum@ProposalDatum{..} ->
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

  initialProposals <- take 2 <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let initialDesiredTerms = zip initialProposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap initialProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap initialProposals $ \(ref,Just ProposalDatum{..}) ->
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
      , inputs = flip map initialDesiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap initialDesiredTerms $ 
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

  claimSlot <- awaitTime (slotToPosixTime 1500) >> currentSlot

  remProposals <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let remDesiredTerms = zip remProposals $ repeat (0 :: Int)

  actives <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  let contractOutputs = flip map remDesiredTerms $ 
        \((ref,Just pd),idx) ->
          let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
          in Output
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

      premiumOutputs = flip map remDesiredTerms $
        \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
          let ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
              Terms{premium} = possibleTerms !! idx
          in Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ uncurry PV2.singleton (unPremiumAsset premiumAsset) $ premium - 1 ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }

      executionOutputs = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
        if expiration == slotToPosixTime 1000 then [] else
          [ Output
              { outputAddress = toCardanoApiAddress paymentAddress
              , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                  [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                      requiredAmount offerQuantity strikePrice 
                  ]
              , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

      extraOutput =
        [ Output
            { outputAddress = buyerPersonalAddr
            , outputValue = utxoValue 5_000_000 mempty
            , outputDatum = NoOutputDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
        ]

  -- Try the composition.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey,buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap remProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap remProposals $ \(ref,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  if expiration == slotToPosixTime 1000 then
                    [ (unOfferBeacon offerBeacon, -1)
                    , (unAskBeacon askBeacon, -1)
                    , (unTradingPairBeacon tradingPairBeacon, -1)
                    , (unContractId contractId, -1)
                    ]
                  else
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
          [ flip map remDesiredTerms $ \((ref,_),idx) ->
              Input
                { inputId = ref
                , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                    toRedeemer $ PurchaseContract $ fromIntegral idx
                }
          , flip map actives $ \(ref,Just ActiveDatum{expiration}) ->
              if expiration == slotToPosixTime 1000 then
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer CloseExpiredContract
                  }
              else
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer ExecuteContract
                  }
          ]
      , outputs = mconcat
          [ extraOutput
          , premiumOutputs
          , extraOutput
          , contractOutputs
          , extraOutput
          , executionOutputs
          ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey,writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Just 2000
          }
      }

-- | When executing a contract, closing an expired contract, and purchasing a Proposal UTxO all in 
-- the same transaction; there is an unrelated output between all of the required outputs. The
-- required outputs are: premium, then contracts, then execution. The first execution output is
-- invalid.
failureTest6 :: MonadEmulator m => m ()
failureTest6 = do
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
                { expiration = slotToPosixTime 2000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }
      proposalDatum3 = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 3000
                , strikePrice = Fraction (1,1)
                , premium = 20
                }
            ]
        }

      proposalDatums = [proposalDatum1, proposalDatum2, proposalDatum3]

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
              { mintTokens = flip concatMap proposalDatums $ 
                  \ProposalDatum{..} ->
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
      , outputs = flip map proposalDatums $ \datum@ProposalDatum{..} ->
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

  initialProposals <- take 2 <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let initialDesiredTerms = zip initialProposals $ repeat (0 :: Int)

  -- Try to buy the Proposal UTxO.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap initialProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap initialProposals $ \(ref,Just ProposalDatum{..}) ->
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
      , inputs = flip map initialDesiredTerms $ \((ref,_),idx) ->
          Input
            { inputId = ref
            , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                toRedeemer $ PurchaseContract $ fromIntegral idx
            }
      , outputs = flip concatMap initialDesiredTerms $ 
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

  claimSlot <- awaitTime (slotToPosixTime 1500) >> currentSlot

  remProposals <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ProposalDatum optionsAddress
  let remDesiredTerms = zip remProposals $ repeat (0 :: Int)

  actives <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  let contractOutputs = flip map remDesiredTerms $ 
        \((ref,Just pd),idx) ->
          let datum@ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
          in Output
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

      premiumOutputs = flip map remDesiredTerms $
        \((ref,Just pd@ProposalDatum{possibleTerms,premiumAsset}),idx) ->
          let ActiveDatum{..} = createActiveDatumFromProposal idx ref pd 
              Terms{premium} = possibleTerms !! idx
          in Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ uncurry PV2.singleton (unPremiumAsset premiumAsset) premium ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
                , outputReferenceScript = toReferenceScript Nothing
                }

      executionOutputs = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
        if expiration == slotToPosixTime 1000 then [] else
          [ Output
              { outputAddress = toCardanoApiAddress paymentAddress
              , outputValue = utxoValue (fromIntegral contractDeposit) $ mconcat
                  [ uncurry PV2.singleton (unAskAsset askAsset) $ 
                      requiredAmount offerQuantity strikePrice - 1
                  ]
              , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconId,contractId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

      extraOutput =
        [ Output
            { outputAddress = buyerPersonalAddr
            , outputValue = utxoValue 5_000_000 mempty
            , outputDatum = NoOutputDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
        ]

  -- Try the composition.
  void $ transact buyerPersonalAddr [refScriptAddress,optionsAddress] [writerPayPrivKey,buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap remProposals $ \(_,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap remProposals $ \(ref,Just ProposalDatum{..}) ->
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
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  if expiration == slotToPosixTime 1000 then
                    [ (unOfferBeacon offerBeacon, -1)
                    , (unAskBeacon askBeacon, -1)
                    , (unTradingPairBeacon tradingPairBeacon, -1)
                    , (unContractId contractId, -1)
                    ]
                  else
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
          [ flip map remDesiredTerms $ \((ref,_),idx) ->
              Input
                { inputId = ref
                , inputWitness = SpendWithPlutusReference optionsRef InlineDatum $ 
                    toRedeemer $ PurchaseContract $ fromIntegral idx
                }
          , flip map actives $ \(ref,Just ActiveDatum{expiration}) ->
              if expiration == slotToPosixTime 1000 then
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer CloseExpiredContract
                  }
              else
                Input
                  { inputId = ref
                  , inputWitness = 
                      SpendWithPlutusReference optionsRef InlineDatum $ 
                        toRedeemer ExecuteContract
                  }
          ]
      , outputs = mconcat
          [ extraOutput
          , premiumOutputs
          , extraOutput
          , contractOutputs
          , extraOutput
          , executionOutputs
          ]
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey,writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Just 2000
          }
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all miscellaneous test scenarios for Active UTxOs.
tests :: TestTree
tests = testGroup "Misc Active UTxO Tests"
  [ mustSucceed "regressionTest1" regressionTest1
  , mustSucceed "regressionTest2" regressionTest2
  , mustSucceed "regressionTest3" regressionTest3
  , mustSucceed "regressionTest4" regressionTest4
  , mustSucceed "regressionTest5" regressionTest5

  , scriptMustFailWithError "failureTest1" 
      "UTxO does not have the right beacons"
      failureTest1
  , scriptMustFailWithError "failureTest2" 
      "Premium output missing premium"
      failureTest2
  , scriptMustFailWithError "failureTest3" 
      "Execution payment output has wrong value"
      failureTest3
  , scriptMustFailWithError "failureTest4" 
      "UTxO does not have the right beacons"
      failureTest4
  , scriptMustFailWithError "failureTest5" 
      "Premium output missing premium"
      failureTest5
  , scriptMustFailWithError "failureTest6" 
      "Execution payment output has wrong value"
      failureTest6
  ]
