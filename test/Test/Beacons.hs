{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Beacons where

import Test.Tasty (TestTree,testGroup)
import Test.Tasty.HUnit

import CardanoOptions

import Test.Prelude

-------------------------------------------------
-- Beacon Name Tests
-------------------------------------------------
-- | The AskBeacon, OfferBeacon, and PremiumBeacon must have different names even if they are for
-- the same asset. The asset is not ada.
nameTest1 :: TestTree
nameTest1 = testCase "nameTest1" $ assertBool "The beacon names are the same" $
    and [ offer /= ask
        , offer /= premium
        , ask /= premium
        ]
  where
    asset = (testTokenSymbol,"Test")
    offer = unOfferBeacon $ genOfferBeaconName $ OfferAsset asset
    ask = unAskBeacon $ genAskBeaconName $ AskAsset asset
    premium = unPremiumBeacon $ genPremiumBeaconName $ PremiumAsset asset

-- | The AskBeacon, OfferBeacon, and PremiumBeacon must have different names even if they are for
-- the same asset. The asset is ada.
nameTest2 :: TestTree
nameTest2 = testCase "nameTest2" $ assertBool "The beacon names are the same" $
    and [ offer /= ask
        , offer /= premium
        , ask /= premium
        ]
  where
    asset = ("","")
    offer = unOfferBeacon $ genOfferBeaconName $ OfferAsset asset
    ask = unAskBeacon $ genAskBeaconName $ AskAsset asset
    premium = unPremiumBeacon $ genPremiumBeaconName $ PremiumAsset asset

-- | The Trading pair beacon does not match any of the other beacons when ada is not part of the
-- trading pair. The premium asset is also not ada.
nameTest3 :: TestTree
nameTest3 = testCase "nameTest3" $ assertBool "The beacon names are the same" $
    and [ pair /= offer
        , pair /= premium
        , pair /= ask
        ]
  where
    offerAsset = OfferAsset (testTokenSymbol,"test1")
    askAsset = AskAsset (testTokenSymbol,"test2")
    premiumAsset = PremiumAsset (testTokenSymbol,"test3")
    offer = unOfferBeacon $ genOfferBeaconName offerAsset
    ask = unAskBeacon $ genAskBeaconName askAsset
    premium = unPremiumBeacon $ genPremiumBeaconName premiumAsset
    pair = unTradingPairBeacon $ genTradingPairBeaconName offerAsset askAsset

-- | The Trading pair beacon does not match any of the other beacons when ada is the offer asset.
-- The premium asset is not ada.
nameTest4 :: TestTree
nameTest4 = testCase "nameTest4" $ assertBool "The beacon names are the same" $
    and [ pair /= offer
        , pair /= premium
        , pair /= ask
        ]
  where
    offerAsset = OfferAsset ("","")
    askAsset = AskAsset (testTokenSymbol,"test2")
    premiumAsset = PremiumAsset (testTokenSymbol,"test3")
    offer = unOfferBeacon $ genOfferBeaconName offerAsset
    ask = unAskBeacon $ genAskBeaconName askAsset
    premium = unPremiumBeacon $ genPremiumBeaconName premiumAsset
    pair = unTradingPairBeacon $ genTradingPairBeaconName offerAsset askAsset

-- | The Trading pair beacon does not match any of the other beacons when ada is the ask asset.
-- The premium asset is not ada.
nameTest5 :: TestTree
nameTest5 = testCase "nameTest5" $ assertBool "The beacon names are the same" $
    and [ pair /= offer
        , pair /= premium
        , pair /= ask
        ]
  where
    offerAsset = OfferAsset (testTokenSymbol,"test1")
    askAsset = AskAsset ("","")
    premiumAsset = PremiumAsset (testTokenSymbol,"test3")
    offer = unOfferBeacon $ genOfferBeaconName offerAsset
    ask = unAskBeacon $ genAskBeaconName askAsset
    premium = unPremiumBeacon $ genPremiumBeaconName premiumAsset
    pair = unTradingPairBeacon $ genTradingPairBeaconName offerAsset askAsset

-- | The Trading pair beacon does not match any of the other beacons when ada is not part of the
-- trading pair. The premium asset is ada.
nameTest6 :: TestTree
nameTest6 = testCase "nameTest6" $ assertBool "The beacon names are the same" $
    and [ pair /= offer
        , pair /= premium
        , pair /= ask
        ]
  where
    offerAsset = OfferAsset (testTokenSymbol,"test1")
    askAsset = AskAsset (testTokenSymbol,"test2")
    premiumAsset = PremiumAsset ("","")
    offer = unOfferBeacon $ genOfferBeaconName offerAsset
    ask = unAskBeacon $ genAskBeaconName askAsset
    premium = unPremiumBeacon $ genPremiumBeaconName premiumAsset
    pair = unTradingPairBeacon $ genTradingPairBeaconName offerAsset askAsset

-- | The ContractID name does not match any of the other beacons even if the same input is used for
-- all of them.
nameTest7 :: TestTree
nameTest7 = testCase "nameTest7" $ assertBool "The beacon names are the same" $
    and [ contract /= offer
        , contract /= premium
        , contract /= ask
        ]
  where
    asset = (testTokenSymbol,"0")
    offer = unOfferBeacon $ genOfferBeaconName $ OfferAsset asset
    ask = unAskBeacon $ genAskBeaconName $ AskAsset asset
    premium = unPremiumBeacon $ genPremiumBeaconName $ PremiumAsset asset
    contract = unContractId $ genContractId $ TxOutRef (TxId $ unCurrencySymbol testTokenSymbol) 0

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all beacon uniqueness tests.
tests :: TestTree
tests = testGroup "Beacon Names"
  [ nameTest1
  , nameTest2
  , nameTest3
  , nameTest4
  , nameTest5
  , nameTest6
  , nameTest7
  ]
