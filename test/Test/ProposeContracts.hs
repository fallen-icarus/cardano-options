{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Test.ProposeContracts
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Ledger.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash)
import Ledger.TimeSlot

import Test.Common
import CardanoOptions

-------------------------------------------------
-- Propose Contract(s) Scenarios
-------------------------------------------------
successfullyProposeSingleContract :: EmulatorTrace ()
successfullyProposeSingleContract = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

successfullyProposeMultipleContracts :: EmulatorTrace ()
successfullyProposeMultipleContracts = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",2)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          , ( Just proposeDatum{premium = 5_000_000}
            , lovelaceValueOf 3_000_000
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      , proposeAsInline = True
      }

mintTokenWithDifferentName :: EmulatorTrace ()
mintTokenWithDifferentName = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Prosed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Prosed" 1
           )
          ]
      , proposeAsInline = True
      }

mintAdditionalTokens :: EmulatorTrace ()
mintAdditionalTokens = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1),("Other",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

burnOtherTokens :: EmulatorTrace ()
burnOtherTokens = do
  h1 <- activateContractWallet (knownWallet 4) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 4)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1),("Active",-1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

beaconGoesToNonDappAddress :: EmulatorTrace ()
beaconGoesToNonDappAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential alwaysSucceedValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

atLeastOneBeaconGoesToNonDappAddress :: EmulatorTrace ()
atLeastOneBeaconGoesToNonDappAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",2)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      , proposeAsInline = True
      }

beaconGoesToNonStakingDappAddress :: EmulatorTrace ()
beaconGoesToNonStakingDappAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     Nothing
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

receivingAddressDidNotApprove :: EmulatorTrace ()
receivingAddressDidNotApprove = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 3)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

datumHasWrongBeaconSymbol :: EmulatorTrace ()
datumHasWrongBeaconSymbol = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = adaSymbol
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

datumHasWrongCurrentAsset :: EmulatorTrace ()
datumHasWrongCurrentAsset = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = testToken1
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

datumHasNegativeQuantity :: EmulatorTrace ()
datumHasNegativeQuantity = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = -100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

datumHasNegativeStrikePrice :: EmulatorTrace ()
datumHasNegativeStrikePrice = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio (-1) 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

datumHasNegativePremium :: EmulatorTrace ()
datumHasNegativePremium = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = -10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

datumHasNegativeTime :: EmulatorTrace ()
datumHasNegativeTime = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = -10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

datumHasWrongDesiredAsset :: EmulatorTrace ()
datumHasWrongDesiredAsset = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken2
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

datumIsNotProposedContract :: EmulatorTrace ()
datumIsNotProposedContract = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

datumIsNotInline :: EmulatorTrace ()
datumIsNotInline = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = False
      }

atLeastOneDatumNotProposedContact :: EmulatorTrace ()
atLeastOneDatumNotProposedContact = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      otherDatum = AssetsForContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",2)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          , ( Just otherDatum
            , lovelaceValueOf 3_000_000
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      , proposeAsInline = True
      }

singleContractNotStoredWithMinDeposit :: EmulatorTrace ()
singleContractNotStoredWithMinDeposit = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 2_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

atLeastOneContractNotStoredWithMinDeposit :: EmulatorTrace ()
atLeastOneContractNotStoredWithMinDeposit = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",2)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          , ( Just proposeDatum{premium = 5_000_000}
            , lovelaceValueOf 2_000_000
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
            )
          ]
      , proposeAsInline = True
      }

burnRedeemerUsedToMint :: EmulatorTrace ()
burnRedeemerUsedToMint = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",1)]
      , proposeBeaconRedeemer = BurnBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 1
           )
          ]
      , proposeAsInline = True
      }

batchedProposedBeaconsMintedToDappAddress :: EmulatorTrace ()
batchedProposedBeaconsMintedToDappAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",2)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          [ ( Just proposeDatum
            , lovelaceValueOf 3_000_000 
           <> singleton optionsBeaconPolicySym1 "Proposed" 2
            )
          ]
      , proposeAsInline = True
      }

batchedProposedBeaconsMintedToWrongAddress :: EmulatorTrace ()
batchedProposedBeaconsMintedToWrongAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let proposeDatum = ProposedContract
        { beaconSymbol = optionsBeaconPolicySym1
        , currentAsset = (adaSymbol,adaToken)
        , currentAssetQuantity = 100_000_000
        , desiredAsset = testToken1
        , strikePrice = unsafeRatio 1 10
        , creatorAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , premiumAsset = (adaSymbol,adaToken)
        , premium = 10_000_000
        , expiration = slotToBeginPOSIXTime def 10
        }
      addr = Address (ScriptCredential optionsValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"propose-contract(s)" h1 $
    ProposeParams
      { proposeBeaconsMinted = [("Proposed",2)]
      , proposeBeaconRedeemer = MintProposedBeacons
      , proposeBeaconPolicy = optionsBeaconPolicy1
      , proposeAddress = addr
      , proposeInfo = 
          []
      , proposeAsInline = True
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Propose Contract(s)"
    [ checkPredicateOptions opts "Fail if beacon token name is not 'Proposed'"
        (Test.not assertNoFailedTransactions) mintTokenWithDifferentName
    , checkPredicateOptions opts "Fail if additional tokens minted"
        (Test.not assertNoFailedTransactions) mintAdditionalTokens
    , checkPredicateOptions opts "Fail if other tokens burned"
        (Test.not assertNoFailedTransactions) burnOtherTokens
    , checkPredicateOptions opts "Fail if only minted beacon goes to non-dapp address"
        (Test.not assertNoFailedTransactions) beaconGoesToNonDappAddress
    , checkPredicateOptions opts "Fail if at least one minted beacon goes to non-dapp address"
        (Test.not assertNoFailedTransactions) atLeastOneBeaconGoesToNonDappAddress
    , checkPredicateOptions opts "Fail if beacon goes to non-staking dapp address"
        (Test.not assertNoFailedTransactions) beaconGoesToNonStakingDappAddress
    , checkPredicateOptions opts "Fail if receiving address did not approve"
        (Test.not assertNoFailedTransactions) receivingAddressDidNotApprove
    , checkPredicateOptions opts "Fail if ProposedContract datum has wrong beacon symbol"
        (Test.not assertNoFailedTransactions) datumHasWrongBeaconSymbol
    , checkPredicateOptions opts "Fail if ProposedContract datum has wrong currentAsset"
        (Test.not assertNoFailedTransactions) datumHasWrongCurrentAsset
    , checkPredicateOptions opts "Fail if ProposedContract datum has negative currentAssetQuantity"
        (Test.not assertNoFailedTransactions) datumHasNegativeQuantity
    , checkPredicateOptions opts "Fail if datum has wrong desiredAsset"
        (Test.not assertNoFailedTransactions) datumHasWrongDesiredAsset
    , checkPredicateOptions opts "Fail if ProposedContract datum has negative strikePrice"
        (Test.not assertNoFailedTransactions) datumHasNegativeStrikePrice
    , checkPredicateOptions opts "Fail if ProposedContract datum has negative premium"
        (Test.not assertNoFailedTransactions) datumHasNegativePremium
    , checkPredicateOptions opts "Fail if ProposedContract datum has negative expiration"
        (Test.not assertNoFailedTransactions) datumHasNegativeTime
    , checkPredicateOptions opts "Fail if beacon stored with wrong datum type"
        (Test.not assertNoFailedTransactions) datumIsNotProposedContract
    , checkPredicateOptions opts "Fail if beacon stored with non-inline datum"
        (Test.not assertNoFailedTransactions) datumIsNotInline
    , checkPredicateOptions opts "Fail if at least one beacon stored with wrong datum type"
        (Test.not assertNoFailedTransactions) atLeastOneDatumNotProposedContact
    , checkPredicateOptions opts "Fail if single contract not stored with minimum deposit"
        (Test.not assertNoFailedTransactions) singleContractNotStoredWithMinDeposit
    , checkPredicateOptions opts "Fail if at least one contract not stored with minimum deposit"
        (Test.not assertNoFailedTransactions) atLeastOneContractNotStoredWithMinDeposit
    , checkPredicateOptions opts "Fail if proposed beacons are grouped into single UTxO at dApp address"
        (Test.not assertNoFailedTransactions) batchedProposedBeaconsMintedToDappAddress
    , checkPredicateOptions opts "Fail if proposed beacons are grouped into single UTxO at wrong address"
        (Test.not assertNoFailedTransactions) batchedProposedBeaconsMintedToWrongAddress

    , checkPredicateOptions opts "Successfully propose sinlge contract"
        assertNoFailedTransactions successfullyProposeSingleContract
    , checkPredicateOptions opts "Successfully propose multiple contracts"
        assertNoFailedTransactions successfullyProposeMultipleContracts
    
    , checkPredicateOptions opts "Fail if BurnBeacons redeemer used to mint"
        (Test.not assertNoFailedTransactions) burnRedeemerUsedToMint
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig batchedProposedBeaconsMintedToDappAddress