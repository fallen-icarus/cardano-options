{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE BangPatterns          #-}


module CardanoOptions
(
  OptionsDatum(..),
  OptionsRedeemer(..),
  OptionsBeaconRedeemer(..),
  CurrencySymbol(..),
  TokenName(..),
  Credential(..),
  POSIXTime(..),
  Slot(..),
  txIdAsToken,
  adaSymbol,
  adaToken,

  optionsValidator,
  optionsValidatorScript,
  optionsValidatorHash,

  optionsBeaconPolicy,
  optionsBeaconPolicyScript,
  optionsBeaconPolicySymbol,
) where

import Data.Aeson hiding (Value,Options)
import qualified Data.Aeson as Aeson
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import Prelude (IO,FilePath,seq) 
import qualified Prelude as Haskell
import Data.String (fromString)
import Data.Text (Text,pack)
import GHC.Generics (Generic)

import Cardano.Api hiding (Script,Value,TxOut,Address,ScriptHash,TxId)
import Cardano.Api.Shelley (PlutusScript (..))
import Ledger.Tx.CardanoAPI.Internal
import Plutus.V2.Ledger.Contexts
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude
import Ledger.Address
import Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts
import Ledger.Bytes (fromHex)
import qualified Plutonomy
import Ledger.Value (valueOf,flattenValue)
import PlutusTx.Numeric as Num
import PlutusTx.Ratio as Ratio
import PlutusPrelude (foldl')
import qualified PlutusTx.AssocMap as Map
import Ledger.TimeSlot
import Ledger.Slot
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import Plutus.V1.Ledger.Bytes (encodeByteString)
import Ledger.Ada (lovelaceValueOf)

-------------------------------------------------
-- Data Types
-------------------------------------------------
-- | Extra parameter for the spending script.
-- This helps ensure only the target assets are swapped and creates a unique beacon for
-- each trading pair.
data OptionsConfig = OptionsConfig
  { currentAssetConfig :: (CurrencySymbol,TokenName)
  , desiredAssetConfig :: (CurrencySymbol,TokenName)
  }

data OptionsDatum 
  = AssetsForContract -- ^ The datum for the UTxO containing the assets for the contract.
      { beaconSymbol :: CurrencySymbol -- ^ Policy Id for the options beaocn policy.
      , currentAsset :: (CurrencySymbol,TokenName)
      , currentAssetQuantity :: Integer
      }
  | ProposedContract
      { beaconSymbol :: CurrencySymbol -- ^ Policy Id for the options beacon policy.
      , currentAsset :: (CurrencySymbol,TokenName)
      , currentAssetQuantity :: Integer
      , desiredAsset :: (CurrencySymbol,TokenName)
      , strikePrice :: Rational
      , creatorAddress :: Address
      , premiumAsset :: (CurrencySymbol,TokenName)
      , premium :: Integer -- ^ Always in ADA
      , expiration :: POSIXTime -- ^ Slot where the contract will expire.
      }
  | ActiveContract
      { beaconSymbol :: CurrencySymbol -- ^ PolicyId for the options beacon policy.
      , currentAsset :: (CurrencySymbol,TokenName)
      , currentAssetQuantity :: Integer
      , desiredAsset :: (CurrencySymbol,TokenName)
      , strikePrice :: Rational
      , creatorAddress :: Address
      , premiumAsset :: (CurrencySymbol,TokenName)
      , premium :: Integer -- ^ This field is kept for easy auditing.
      , expiration :: POSIXTime -- ^ Slot where the contract will expire.
      , contractId :: TokenName -- ^ Tx hash of the AssetsForContract UTxO used to create this UTxO.
      }
  deriving (Haskell.Show,Generic)

instance Eq OptionsDatum where
  {-# INLINABLE (==) #-}
  (AssetsForContract a b c) == (AssetsForContract a' b' c') = 
    a == a' && b == b' && c == c'
  (ProposedContract a b c d e f g h i) == (ProposedContract a' b' c' d' e' f' g' h' i') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i'
  (ActiveContract a b c d e f g h i j) == (ActiveContract a' b' c' d' e' f' g' h' i' j') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && 
    i == i' && j == j'
  _ == _ = False

data OptionsRedeemer
  = CloseAssets -- ^ Close an unused AssetsForContract UTxO.
  | CloseProposedContracts
  | AcceptContract
  | ExecuteContract TokenName -- ^ The contract ID of the contract to be executed.
  | CloseExpiredContract -- ^ Reclaim assets after a contract expires without being executed.
  deriving (Haskell.Show,Generic)

data OptionsBeaconRedeemer
  -- | Mint the AssetsBeacon for the AssetsForContract UTxO.
  = MintAssetsBeacon
  -- | Mint the beacons for proposed contracts.
  | MintProposedBeacons
  -- | Mint the Active beacon and ContractId for an accepted contract.
  | MintActiveBeacon
      TokenName -- ^ Tx hash of the AssetsForContract UTxO used to accept the contract.
      Credential 
        -- ^ The staking credential for the options address being used. Either a script or pubkey.
  | BurnBeacons
  deriving (Haskell.Show,Generic)

-- | A helper type used to create testing beacons.
type AppName = BuiltinString

PlutusTx.unstableMakeIsData ''OptionsDatum
PlutusTx.unstableMakeIsData ''OptionsRedeemer
PlutusTx.unstableMakeIsData ''OptionsBeaconRedeemer

PlutusTx.makeLift ''OptionsConfig

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Used to create a testing set of beacons/IDs without having to change the logic.
app :: AppName
app = "testing"

{-# INLINABLE encodeDatum #-}
-- | This is a convenient way to check what kind of datum it is.
encodeDatum :: OptionsDatum -> Integer
encodeDatum AssetsForContract{} = 0
encodeDatum ProposedContract{} = 1
encodeDatum ActiveContract{} = 2

{-# INLINABLE parseOptionsDatum #-}
parseOptionsDatum :: OutputDatum -> OptionsDatum
parseOptionsDatum d = case d of
  (OutputDatum (Datum d')) -> unsafeFromBuiltinData d'
  _ -> traceError "All options datums must be inline datums."

{-# INLINABLE tokenNameAsTxId #-}
tokenNameAsTxId :: TokenName -> TxId
tokenNameAsTxId (TokenName n) = TxId n

-- | This function is not used by the contract. It is used for testing with the
-- emulator.
txIdAsToken :: TxId -> TokenName
txIdAsToken (TxId n) = TokenName n

{-# INLINABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput (ScriptContext info (Spending ref)) = getScriptInput (txInfoInputs info) ref
ownInput _ = traceError "script input error"

{-# INLINABLE getScriptInput #-}
getScriptInput :: [TxInInfo] -> TxOutRef -> TxOut
getScriptInput [] _ = traceError "script input error"
getScriptInput ((TxInInfo iRef ot) : tl) ref
  | iRef == ref = ot
  | otherwise = getScriptInput tl ref

{-# INLINABLE signed #-}
signed :: [PubKeyHash] -> PubKeyHash -> Bool
signed [] _ = False
signed (k:ks) k'
  | k == k' = True
  | otherwise = signed ks k'

-- | This is only used by the validator to prevent permanent locking when a staking script
-- is accidentally used. The beacons require that the address uses a staking pubkey.
{-# INLINABLE stakingCredApproves #-}
stakingCredApproves :: Address -> TxInfo -> Bool
stakingCredApproves addr info = case addressStakingCredential addr of
  -- | This is to prevent permanent locking of funds.
  -- The dApp is not meant to be used without a staking credential.
  Nothing -> True

  -- | Check if staking credential signals approval.
  Just stakeCred@(StakingHash cred) -> case cred of
    PubKeyCredential pkh -> signed (txInfoSignatories info) pkh
    ScriptCredential _ -> isJust $ Map.lookup stakeCred $ txInfoWdrl info
  
  Just _ -> traceError "Wrong kind of staking credential."

-- | This function does the input validation for MintActiveBeacon of the beacon policy.
-- It will either fail with an appropriate error or return what the output datum should
-- be based on the two inputs.
{-# INLINABLE acceptContractInputCheck #-}
acceptContractInputCheck :: CurrencySymbol -> TokenName -> ValidatorHash 
                         -> Credential -> TxInfo -> OptionsDatum
acceptContractInputCheck beaconSym contractId valHash stakingCred info =
    datumsAgree `seq`
      ActiveContract
        { beaconSymbol = beaconSymbol proposedDatum
        , currentAsset = currentAsset proposedDatum
        , currentAssetQuantity = currentAssetQuantity proposedDatum
        , desiredAsset = desiredAsset proposedDatum
        , strikePrice = strikePrice proposedDatum
        , creatorAddress = creatorAddress proposedDatum
        , premiumAsset = premiumAsset proposedDatum
        , premium = premium proposedDatum
        , expiration = expiration proposedDatum
        , contractId = contractId
        }
  where
    -- | This function traverses the inputs and finds the two required inputs.
    -- This will fail if with the appropriate error message.
    (!assetsDatum,!proposedDatum) =
      let inputs = txInfoInputs info
          foo
            targetId 
            acc@(!ad,!pd) 
            TxInInfo{txInInfoOutRef = TxOutRef{txOutRefId = txId}
                    ,txInInfoResolved =
                      TxOut{txOutAddress = Address{addressCredential = addrCred
                                                  ,addressStakingCredential = maybeStakeCred
                                                  }
                           ,txOutValue = iVal
                           ,txOutDatum = d
                           }
                    } =
            -- | Check if this is input is from a dApp address.
            if addrCred == ScriptCredential valHash then
              -- | Check if this input is from the proper dApp address.
              if maybeStakeCred == Just (StakingHash stakingCred) then
                -- | Check if the input is an Assets input. It will have an Assets beacon.
                if valueOf iVal beaconSym (TokenName "Assets") == 1 then
                  -- | Check if the tx id matches the contractID.
                  if targetId == txId then
                    -- | Check if another Assets input has been found already.
                    if not $ isJust ad then
                      -- | Return the datum.
                      (Just $ parseOptionsDatum d, pd)
                    else traceError "Only one Assets input allowed in tx"
                  else traceError "ContractID does not match Assets input tx hash"
                -- | Since it wasn't an Assets input, check if it is a Proposed input.
                -- It would have one Proposed beacon.
                else if valueOf iVal beaconSym (TokenName "Proposed") == 1 then
                  -- | Check if another Proposed input has been found already.
                  if not $ isJust pd then
                    -- | Return the datum.
                    (ad,Just $ parseOptionsDatum d)
                  else traceError "Only one Proposed input allowed in tx"
                else traceError "Invalid dApp address input found"
              else traceError "Input from different dApp address found"
            else acc -- ^ Skip this input.
      in case foldl' (foo (tokenNameAsTxId contractId)) (Nothing,Nothing) inputs of
        (Nothing,Nothing) -> traceError "Inputs not found"
        (Nothing,Just _) -> traceError "Missing Assets UTxO"
        (Just _,Nothing) -> traceError "Missing Proposed UTxO"
        (Just ad, Just pd) -> (ad,pd)

    -- | Check whether the datums agree. Either fails with error or returns True.
    datumsAgree :: Bool
    datumsAgree
      | beaconSymbol assetsDatum /= beaconSymbol proposedDatum =
          traceError "Datums do not agree on beaconSymbol"
      | currentAsset assetsDatum /= currentAsset proposedDatum =
          traceError "Datums do not agree on currentAsset"
      | currentAssetQuantity assetsDatum /= currentAssetQuantity proposedDatum =
          traceError "Datums do not agree on currentAssetQuantity"
      | otherwise = True

-------------------------------------------------
-- On-Chain Options Validator
-------------------------------------------------
-- | This validator guarantees trustless contract negotiations and executions. Each user
-- gets there own address instance for this validator script. Due to this, custody and
-- delegation control is maintained by every user. This validator script overloads the
-- staking credential so that it can be used to authorize owner related actions.
--
-- This validator follows the American rules for options contracts where the contract can 
-- be executed at any time before the expiration.
mkOptionsValidator :: OptionsConfig -- ^ Extra Parameter
                   -> OptionsDatum -> OptionsRedeemer -> ScriptContext -> Bool
mkOptionsValidator OptionsConfig{..} optionsDatum r ctx@ScriptContext{scriptContextTxInfo=info} = 
  case r of
    CloseAssets ->
      -- | The UTxO must have an AssetsForContract datum.
      traceIfFalse "Datum is not an AssetsForContract datum" (encodeDatum optionsDatum == 0) &&
      -- | All Assets beacons among tx inputs must be burned.
      traceIfFalse "Assets beacons not burned."
        ( valueOf totalInputValue (beaconSymbol optionsDatum) (TokenName "Assets") == 
            Num.negate (valueOf minted (beaconSymbol optionsDatum) (TokenName "Assets"))
        ) &&
      -- | No Active beacons can be minted. This prevents possibly combining redeemers and
      -- producing unexpected behaviors.
      traceIfFalse "No Active beacons can be minted" 
        (valueOf (txInfoMint info) (beaconSymbol optionsDatum) (TokenName "Active") == 0) &&
      -- | The address' staking credential must signal approval.
      traceIfFalse "Staking credential did not approve" stakingCredApproves'
      -- Note: This redeemer can be used to close AssetsForContract UTxOs that are missing
      -- the Assets beacon.
    CloseProposedContracts ->
      -- | The UTxO must have a ProposedContract datum.
      traceIfFalse "Datum is not an ProposedContract datum" (encodeDatum optionsDatum == 1) &&
      -- | All Proposed beacons among tx inputs must be burned.
      traceIfFalse "Proposed beacons not burned."
        ( valueOf totalInputValue (beaconSymbol optionsDatum) (TokenName "Proposed") == 
            Num.negate (valueOf minted (beaconSymbol optionsDatum) (TokenName "Proposed"))
        ) &&
      -- | No Active beacons can be minted. This prevents possibly combining redeemers and
      -- producing unexpected behaviors.
      traceIfFalse "No Active beacons can be minted" 
        (valueOf (txInfoMint info) (beaconSymbol optionsDatum) (TokenName "Active") == 0) &&
      -- | The address' staking credential must signal approval.
      traceIfFalse "Staking credential did not approve" stakingCredApproves'
      -- Note: This redeemer can be used to close ProposedContract UTxOs that are missing
      -- the Proposed beacon.
    AcceptContract ->
      -- | Check that an Active token is minted. This guarantees that the minting policy is
      -- executed this tx. The minting policy does all the required checks.
      traceIfFalse "Active beacon not minted" $
        valueOf (txInfoMint info) (beaconSymbol optionsDatum) (TokenName "Active") == 1
    ExecuteContract contractId' ->
      -- | The UTxO must have an Active beacon. This also implies that the UTxO has
      -- an ActiveContractDatum.
      traceIfFalse "Input does not have an Active beacon" 
        (valueOf inputValue (beaconSymbol optionsDatum) (TokenName "Active") == 1) &&
      -- | The input must have the corresponding contractID.
      traceIfFalse "This input is for a different contract"
        (contractId optionsDatum == contractId') &&
      -- | The contract must not be expired.
      traceIfFalse "Contract is expired" (not $ contractIsExpired executionTime) &&
      -- | Exactly one ContractID must be burned. The other will be used as a receipt of
      -- execution to guarantee unique payments. This prevents double satisfaction despite
      -- composing executions.
      traceIfFalse "One ContractID not burned"
        (valueOf minted (beaconSymbol optionsDatum) contractId' == (-1)) &&
      -- | All Active beacons among tx inputs must be burned.
      traceIfFalse "Active beacons not burned."
        ( valueOf totalInputValue (beaconSymbol optionsDatum) (TokenName "Active") == 
            Num.negate (valueOf minted (beaconSymbol optionsDatum) (TokenName "Active"))
        ) &&
      -- | The following value must be paid to the creator's address:
      --     1) 5 ADA 
      --       <> (uncurry singleton desiredAssetConfig) (ceiling $ currentAssetQuantity * strikePrice)
      --       <> 1 ContractID
      -- The 5 ADA was the creator's deposit from the AssetsForContract UTxO.
      traceIfFalse "Creator not properly paid desired asset" (creatorPaid contractId')
    CloseExpiredContract ->
      -- | The UTxO must have an ActiveContract datum.
      traceIfFalse "Datum is not an ActiveContract datum" (encodeDatum optionsDatum == 2) &&
      -- | The address' staking credential must signal approval.
      traceIfFalse "Staking credential did not approve" stakingCredApproves' &&
      -- | If the Active beacon is present:
      if valueOf inputValue (beaconSymbol optionsDatum) (TokenName "Active") == 1 then
        -- | The contract must be expired.
        traceIfFalse "The contract is still active" (contractIsExpired closeTime) &&
        -- | All ContractIDs among tx inputs must be burned.
        traceIfFalse "ContractIDs not burned."
          ( valueOf totalInputValue (beaconSymbol optionsDatum) (contractId optionsDatum) == 
              Num.negate (valueOf minted (beaconSymbol optionsDatum) (contractId optionsDatum))
          ) &&
        -- | All Active beacons among tx inputs must be burned.
        traceIfFalse "Active beacons not burned."
          ( valueOf totalInputValue (beaconSymbol optionsDatum) (TokenName "Active") == 
              Num.negate (valueOf minted (beaconSymbol optionsDatum) (TokenName "Active"))
          ) 
      -- | Else the conditions are already satisfied by the first two checks.
      else True
      -- Note: This redeemer can be used to close ActiveContract UTxOs that are missing
      -- the Active beacon.
        
  where
    -- | Get the credential for this input as well as its value.
    -- Credential is used to check asset flux for address and ensure staking credential approves 
    -- when necessary. The value is used to quickly check for beacon tokens.
    (inputCredentials,inputValue) = 
      let TxOut{txOutAddress=addr,txOutValue=iVal} = ownInput ctx
      in (addr,iVal)

    -- | This tends to build up a thunk so its evaluation is forced even though it is not always
    -- needed.
    stakingCredApproves' :: Bool
    !stakingCredApproves' = stakingCredApproves inputCredentials info

    totalInputValue :: Value
    totalInputValue = valueSpent info

    minted :: Value
    minted = txInfoMint info

    -- | Get the execution time from the tx's validity range.
    -- It uses to upper bound of the tx's validity range so that a user can't
    -- set an earlier time than has already passed to trick the script.
    executionTime :: POSIXTime
    executionTime = case (\(UpperBound t _) -> t) $ ivTo $ txInfoValidRange info of
      PosInf -> traceError "invalid-hereafter not specified"
      Finite t -> t
      _ -> traceError "Shouldn't be NegInf."

    -- | Get the close time from the tx's validity range. Based off the lower bound.
    -- This is used when a creator is claiming that a contract has expired.
    closeTime :: POSIXTime
    closeTime = case (\(LowerBound t _) -> t) $ ivFrom $ txInfoValidRange info of
      NegInf -> traceError "invalid-before not specified"
      Finite x -> x
      _ -> traceError "Shouldn't be PosInf."

    -- | Check if the expiration has passed. Only used with active datums.
    contractIsExpired :: POSIXTime -> Bool
    contractIsExpired currentTime = currentTime > expiration optionsDatum

    -- | Always rounds up. This is used when calculating the amount owed to the creator
    -- when a contract is executed.
    ceiling :: Rational -> Integer
    ceiling rat
      | fromInteger i' < rat = i' + 1
      | otherwise = i'
      where
        i' = truncate rat

    -- | Checks whether the creator was paid when a contract is executed. If there are two identical
    -- contract payments due (same value and target address), a double satisfaction can occur if
    -- the payments are made in the same tx. To prevent this, every payment must be guaranteed to
    -- be unique. This can be achieved by requiring the non-burned ContractID to be included in the
    -- payment output.
    creatorPaid :: TokenName -> Bool
    creatorPaid contractId' =
      let outputs = txInfoOutputs info
          due = ceiling $ fromInteger (currentAssetQuantity optionsDatum) * strikePrice optionsDatum
          creatorAddr = creatorAddress optionsDatum
          desired = desiredAssetConfig
          receiptToken = (beaconSymbol optionsDatum,contractId')
          foo targetAddr targetAsset due' receiptToken' acc TxOut{txOutValue = oVal
                                                                 ,txOutAddress = addr
                                                                 } =
            -- | Check if the output is to the creator.
            if addr == targetAddr then
              -- | Check if the output has the proper value. The other ContractID must be included
              -- in the payment to guarantee uniqueness of the payment. This uses OR since only
              -- one output needs to be valid. Since acc is initially set to False, this can
              -- only return True if a valid output is present.
              acc ||
                ( oVal == lovelaceValueOf 5_000_000 -- ^ Creator's deposit from AssetsForContract UTxO.
                    <> (uncurry singleton targetAsset) due'
                    <> (uncurry singleton receiptToken') 1
                )
            else acc -- ^ Skip input.
      in foldl' (foo creatorAddr desired due receiptToken) False outputs

data Options
instance ValidatorTypes Options where
  type instance RedeemerType Options = OptionsRedeemer
  type instance DatumType Options = OptionsDatum

optionsValidator :: OptionsConfig -> Validator
optionsValidator config = Plutonomy.optimizeUPLC $ validatorScript $ mkTypedValidator @Options
    ($$(PlutusTx.compile [|| mkOptionsValidator ||])
      `PlutusTx.applyCode` PlutusTx.liftCode config)
    $$(PlutusTx.compile [|| wrap ||])
  where wrap = mkUntypedValidator

optionsValidatorScript :: OptionsConfig -> Script
optionsValidatorScript = unValidatorScript . optionsValidator

optionsValidatorHash :: OptionsConfig -> ValidatorHash
optionsValidatorHash = Scripts.validatorHash . optionsValidator

-------------------------------------------------
-- On-Chain Options Beacon Policy
-------------------------------------------------
-- | This minting policy tags each step of the process with a easily queryable token. 
-- When a contract is accepted, two beacons with the token name of the ContractID are 
-- minted: one stays in the options validator address with the assets that can be swapped
-- upon execution, and the other goes to the user who accepted the contract. This user 
-- controlled copy can be freely traded. To actually execute the contract, both copies
-- of this ContractID tokne must be burned. In this way, the user controlled copy acts 
-- as a key to the contract.
mkOptionsBeaconPolicy :: AppName -> ValidatorHash -- ^ Extra parameters
                      -> OptionsBeaconRedeemer -> ScriptContext -> Bool
mkOptionsBeaconPolicy appName valHash r ctx@ScriptContext{scriptContextTxInfo = info} = case r of
    MintAssetsBeacon -> 
      -- | The following function checks:
      -- 1) Must only mint one token with the token name "Assets".
      -- 2) No other tokens can be minted/burned by this policy.
      mintCheck &&
      -- | The following function checks:
      -- 1) The Assets beacon must go to an address that uses the valHash as the spending credential.
      -- 2) The Assets beacon must go to an address that has a staking credential.
      -- 3) The receiving address must signal approval.
      -- 4) The Assets beacon must be stored in a UTxO with the proper inline datum:
      --     a) The currency symbol must be this policy id.
      --     b) The currentAssetQuantity > 0
      -- 5) The Assets beacon must be stored with the proper value:
      --     a) 5 ADA <> target of current asset <> Asset beacon
      -- 5 ADA is used as the min deposit because a hard coded minUTxO value is easier to protect
      -- than a variable one. By setting it to 5 ADA, most UTxO sizes should be possible while still
      -- allowing the script to easily protect the writer's deposit.
      traceIfFalse "Receiving staking credential did not approve" assetsOrProposedDestinationCheck
    MintProposedBeacons ->
      -- | The following function checks:
      -- 1) Must only mint tokens with the token name "Proposed". Can mint multiple of this token.
      -- 2) No other tokens can be minted by this policy.
      mintCheck &&
      -- | The following function checks:
      -- 1) The Proposed beacons must go to an address that uses the valHash as the spending credential.
      -- 2) The Proposed beacons must go to an address that has a staking credential.
      -- 3) The receiving address must signal approval.
      -- 3) The Proposed beacons must be stored with the proper inline ProposedContract datum:
      --     a) beaconSymbol == this policy id
      --     b) currentAssetQuantity > 0
      --     c) strikePrice > 0
      --     d) premium > 0
      --     e) expiration > 0
      -- 4) Each Proposed beacon must be stored with the value:
      --     a) 3 ADA <> 1 Proposed beacon
      -- Hard coding the minUTxO value to 3 makes it easier to ensure the writer's deposit is returned
      -- when the contract is accepted.
      traceIfFalse "Receiving staking credential did not approve" assetsOrProposedDestinationCheck
    MintActiveBeacon contractId stakingCred ->
      -- ============================
      -- | Beacon Usage Verification:
      -- ============================
      -- | The Proposed beacon must be burned.
      -- | The Assets beacon must be burned.
      -- | One Active beacon and 2 ContractIDs must be minted. The ContractIDs must use the token
      -- name supplied with the MintActiveBeacon redeemer.
      mintCheck &&

      -- =====================
      -- | Input Verification:
      -- =====================
      -- | Only one dApp address has inputs in this tx. Addresses are not meant to compose. The
      -- redeemer stakingCred specifies what dApp address this transaction is for.
      -- | There must be exactly two inputs from this address:
      --     1) One input must be an AssetsForContract UTxO with an Assets beacon.
      --     2) One input must be a ProposedContract UTxO with a Proposed beacon.
      --     3) The AssetsForContract UTxO input must have the tx hash supplied by the
      --        MintActiveBeacon redeemer.
      -- | The datums of the two inputs must agree:
      --     1) same beaconSymbol
      --     2) same currentAsset
      --     3) same currentAssetQuantity

      -- ======================
      -- | Output Verification:
      -- ======================
      -- | The premium must be paid to the creator's address + 3 ADA. The 3 ADA was the creator's
      -- deposit for the ProposedContract UTxO.
      -- | There must be only one output to target address with the following properties:
      --     1) Must have the proper value:
      --          5 ADA <> quantity of current asset <> Active beacon <> 1 ContractID
      --     2) The datum must be an ActiveContract inline datum with:
      --           a) beaconSymbol == beaconSymbol of Proposed input
      --           b) currentAsset == currentAsset of Proposed input
      --           c) currentAssetQuantity == currentAssetQuantity of Proposed input
      --           d) desiredAsset == desiredAsset of Proposed input
      --           e) strikePrice == strikePrice of Proposed input
      --           f) creatorAddress == creatorAddress of Proposed input
      --           g) premiumAsset == premiumAsset of Proposed input
      --           h) premium == premium of Proposed input
      --           i) expiration == expiration of Proposed input
      --           j) contractId == tx hash supplied by AcceptContract redeemer
      activeDestinationCheck stakingCred
        (acceptContractInputCheck beaconSym contractId valHash stakingCred info)
    BurnBeacons ->
      -- | Always allowed as long as this redeemer is only used to burn.
      mintCheck
  where
    beaconSym :: CurrencySymbol
    beaconSym = ownCurrencySymbol ctx

    minted :: Value
    minted = txInfoMint info

    -- | Returns only the beacons minted/burned. This is useful for ensuring only
    -- the required beacons are minting.
    beaconMint :: [(CurrencySymbol,TokenName,Integer)]
    beaconMint = case Map.lookup beaconSym $ getValue minted of
      Nothing -> traceError "MintError"
      Just bs -> flattenValue $ Value $ Map.insert beaconSym bs Map.empty -- ^ a Value with only beacons

    mintCheck :: Bool
    mintCheck = case (r,beaconMint) of
      (MintAssetsBeacon,[(_,tn,n)]) ->
        traceIfFalse "The Assets beacon must have the token name 'Assets'" (tn == TokenName "Assets") &&
        traceIfFalse "Only one Assets beacon can be minted per tx" (n == 1)
      (MintAssetsBeacon, _) -> traceError "This redeemer only allows minting one Assets beacon"
      (MintProposedBeacons, [(_,tn,n)]) ->
        traceIfFalse "The Proposed beacon must have the token name 'Proposed'" 
          (tn == TokenName "Proposed") &&
        traceIfFalse "The Proposed beacon can only be minted with this redeemer" (n > 0)
      (MintProposedBeacons, _) ->
        traceError "This redeemer only allows minting Proposed beacons"
      (MintActiveBeacon contractId _, [_,_,_,_]) ->
        let numAssetsBurned = valueOf minted beaconSym (TokenName "Assets")
            numProposedBurned = valueOf minted beaconSym (TokenName "Proposed")
            numActiveMinted = valueOf minted beaconSym (TokenName "Active")
            numContractIdsMinted = valueOf minted beaconSym contractId
        in -- | The Assets beacon must be burned.
           traceIfFalse "Exactly one Assets beacon not burned" (numAssetsBurned == (-1)) &&
           -- | The Proposed beacon must be burned.
           traceIfFalse "Exactly one Proposed beacon not burned" (numProposedBurned == (-1)) &&
           -- | The Active beacon must be minted.
           traceIfFalse "Exactly one Active beacon not minted" (numActiveMinted == 1) &&
           -- | Two ContractIDs must be minted.
           traceIfFalse "Exactly two ContractIDs not minted" (numContractIdsMinted == 2)
      (MintActiveBeacon _ _, _) ->
        traceError "Wrong combination of beacons minted/burned with this redeemer"
      (BurnBeacons, xs) ->
        traceIfFalse "Beacons can only be burned with this redeemer" (all (\(_,_,n) -> n < 0) xs)
    
    -- | A helper function for destinationCheck to make the code easier to reason about.
    -- This uses the appName in the error message so that it isn't optimized away.
    validDestination :: ValidatorHash -> Bool
    validDestination spendVh
      | spendVh /= valHash = 
          traceError ("Beacon not minted to the proper " <> appName <> " address")
      | otherwise = True

    validDatum :: OptionsBeaconRedeemer -> OptionsDatum -> Bool
    validDatum MintAssetsBeacon AssetsForContract{..}
      | beaconSymbol /= beaconSym = traceError "Invalid AssetsForContract beaconSymbol"
      | currentAssetQuantity <= 0 = traceError "Invalid AssetsForContract currentAssetQuantity"
      | otherwise = True
    validDatum MintAssetsBeacon _ = traceError "Assets beacon stored with wrong datum type"
    validDatum MintProposedBeacons ProposedContract{..}
      | beaconSymbol /= beaconSym = traceError "Invalid ProposedContract beaconSymbol"
      | currentAssetQuantity <= 0 = traceError "Invalid ProposedContract currentAssetQuantity"
      | strikePrice <= fromInteger 0 = traceError "Invalid ProposedContract strikePrice"
      | premium <= 0 = traceError "Invalid ProposedContract premium"
      | expiration <= 0 = traceError "Invalid ProposedContract expiration"
      | otherwise = True
    validDatum MintProposedBeacons _ = traceError "Proposed beacon stored with wrong datum type"
    validDatum _ _ = False -- ^ This function is never called with the other redeemers.

    -- | This checks that the beacons are stored with the proper values. Helps simplify
    -- destinationCheck. Either fails with error or returns True.
    utxoHasProperValue :: Value -> OptionsDatum -> Bool
    utxoHasProperValue oVal AssetsForContract{..} =
      if oVal == lovelaceValueOf 5_000_000
              <> (uncurry singleton currentAsset) currentAssetQuantity
              <> singleton beaconSym (TokenName "Assets") 1
      then True
      else traceError "AssetsForContract UTxO not stored with proper value"
    utxoHasProperValue oVal ProposedContract{} =
      if oVal == lovelaceValueOf 3_000_000 
              <> singleton beaconSym (TokenName "Proposed") 1 -- ^ Only one Proposed beacon per UTxO.
      then True
      else traceError "ProposedContract UTxO not stored with proper value"
    utxoHasProperValue oVal ActiveContract{..} =
      if oVal == lovelaceValueOf 5_000_000
              <> (uncurry singleton currentAsset) currentAssetQuantity
              <> singleton beaconSym (TokenName "Active") 1
              <> singleton beaconSym contractId 1 
                   -- ^ Only one ContractID should be stored with contract.
      then True
      else traceError "ActiveContract UTxO not stored with proper value"
        
    activeDestinationCheck :: Credential -> OptionsDatum -> Bool
    activeDestinationCheck stakingCred datum@ActiveContract{..} =
      let outputs = txInfoOutputs info
          foo 
            acc@(dAppOutput,premiumOutput) 
            TxOut{txOutDatum = d
                 ,txOutValue = oVal
                 ,txOutAddress = addr@Address{addressCredential = addrCred
                                             ,addressStakingCredential = maybeStakeCred
                                             }
                 } =
            -- | Check if the output contains the Active beacon.
            if valueOf oVal beaconSym (TokenName "Active") == 1 then
              -- | Check that the Active beacon is stored at the proper address.
              if addrCred == ScriptCredential valHash && 
                 maybeStakeCred == Just (StakingHash stakingCred) then
                  -- | Return whether or not the dApp output is valid.
                  ( dAppOutput && (parseOptionsDatum d == datum) && utxoHasProperValue oVal datum
                  , premiumOutput
                  )
              else traceError "Active beacon minted to wrong address"
            -- | Check if the address is the premium address.
            else if addr == creatorAddress then
              -- | Return whether or not the premium was properly paid.
              ( dAppOutput
              , oVal == lovelaceValueOf 3_000_000 -- ^ Deposit from the ProposedContract UTxO.
                     <> (uncurry singleton premiumAsset) premium
              )
            -- | Make sure there are no invalid outputs to dApp address.
            else if addrCred == ScriptCredential valHash && 
                 maybeStakeCred == Just (StakingHash stakingCred) then
                    -- | It must have an Active beacon.
                    if valueOf oVal beaconSym (TokenName "Active") /= 1 then
                      traceError "Invalid output to dApp address"
                    else acc
            else acc -- ^ Skip input.
      in case foldl' foo (True,False) outputs of
        (True,True) -> True
        (_,False) -> traceError "Premium not paid"
        (False,_) -> traceError "Invalid ActiveContract datum" 
          -- ^ This can only be false if datum does not match the expected datum.
    activeDestinationCheck _ _ = False -- ^ Never called with other datums.

    -- | This can only return False if the staking credential did not approve. Otherwise, it will
    -- either fail with an error message or return True.
    assetsOrProposedDestinationCheck :: Bool
    assetsOrProposedDestinationCheck =
      let outputs = txInfoOutputs info
          foo acc TxOut{txOutDatum = d
                       ,txOutValue = oVal
                       , txOutAddress = addr@Address{addressCredential = addrCred
                                                    ,addressStakingCredential = maybeStakeCred
                                                    }
                       } =
            case r of
              MintAssetsBeacon ->
                if valueOf oVal beaconSym (TokenName "Assets") == 1 then
                  let datum = parseOptionsDatum d
                  in case (addrCred,maybeStakeCred) of
                    (ScriptCredential vh, Just (StakingHash _)) -> 
                      acc && validDestination vh && validDatum r datum && 
                      utxoHasProperValue oVal datum && stakingCredApproves addr info
                    _ -> traceError "Asset beacon must go to a dApp address with a staking credential"
                else acc
              MintProposedBeacons ->
                if valueOf oVal beaconSym (TokenName "Proposed") == 1 then
                  let datum = parseOptionsDatum d
                  in case (addrCred,maybeStakeCred) of
                    (ScriptCredential vh, Just (StakingHash _)) -> 
                      acc && validDestination vh && validDatum r datum && 
                      utxoHasProperValue oVal datum && stakingCredApproves addr info
                    _ -> traceError "Proposed beacon must go to a dApp address with a staking credential"
                else acc
              _ -> False -- ^ Never used for the other redeemers
      in foldl' foo True outputs
  
optionsBeaconPolicy :: OptionsConfig -> MintingPolicy
optionsBeaconPolicy config = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
  ($$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode app
    `PlutusTx.applyCode` PlutusTx.liftCode valHash)
  where
    wrap x y = mkUntypedMintingPolicy $ mkOptionsBeaconPolicy x y
    valHash = optionsValidatorHash config

optionsBeaconPolicyScript :: OptionsConfig -> Script
optionsBeaconPolicyScript = unMintingPolicyScript . optionsBeaconPolicy

optionsBeaconPolicySymbol :: OptionsConfig -> CurrencySymbol
optionsBeaconPolicySymbol = scriptCurrencySymbol . optionsBeaconPolicy