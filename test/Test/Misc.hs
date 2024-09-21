{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Misc where

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
-- | Execute a contract, close an expired contract, purchase a Proposal UTxO, and create a new
-- Proposal UTxO all in the same transaction. The new Proposal UTxO uses all of the beacons from the
-- one that is purchased.
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

      newProposalDatum = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken4")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 5000
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
      , extraKeyWitnesses = [buyerPubKey,writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Just 2000
          }
      }

-- | Execute a contract, close an expired contract, purchase a Proposal UTxO, and create a new
-- Proposal UTxO all in the same transaction. The new Proposal UTxO uses some of the beacons from the
-- one that is purchased.
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

      newProposalDatum = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken5")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 5000
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
              , mintRedeemer = toRedeemer CreateCloseOrUpdateProposals
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
      , referenceInputs = [proposalBeaconsRef,activeBeaconsRef,optionsRef]
      , extraKeyWitnesses = [buyerPubKey,writerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Just 2000
          }
      }

-- | Execute a contract, close an expired contract, purchase a Proposal UTxO, create a new
-- Proposal UTxO, and update the payment address for a contract all in the same transaction. The 
-- new Proposal UTxO uses some of the beacons from the one that is purchased.
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

      newProposalDatum = unsafeCreateProposalDatum $ NewProposalInfo
        { offerAsset = OfferAsset (testTokenSymbol,"TestToken2")
        , offerQuantity = 10
        , askAsset = AskAsset (testTokenSymbol,"TestToken3")
        , premiumAsset = PremiumAsset (testTokenSymbol,"TestToken5")
        , contractDeposit = 5_000_000
        , paymentAddress = toPlutusAddress writerPersonalAddr
        , possibleTerms =
            [ Terms
                { expiration = slotToPosixTime 5000
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
              , mintRedeemer = toRedeemer CreateCloseOrUpdateProposals
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

-- | Burn the Key NFT by itself.
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

  actives <- txOutRefsAndDatumsAtAddress @ActiveDatum optionsAddress
  
  -- Try to burn the key.
  void $ transact buyerPersonalAddr [refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap actives $ \(_,Just ActiveDatum{..}) ->
                  [ (unContractId contractId, -1) ]
              , mintRedeemer = toRedeemer BurnActiveBeacons
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeBeaconsRef
              }
          ]
      , referenceInputs = [activeBeaconsRef]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all miscellaneous test scenarios.
tests :: TestTree
tests = testGroup "Misc Tests"
  [ mustSucceed "regressionTest1" regressionTest1
  , mustSucceed "regressionTest2" regressionTest2
  , mustSucceed "regressionTest3" regressionTest3
  , mustSucceed "regressionTest4" regressionTest4
  ]
