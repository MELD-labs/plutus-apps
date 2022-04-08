
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

-- Restored from https://github.com/input-output-hk/plutus/pull/4394/files#diff-b9a7887f66620c2328f8ef261e72875c9e9340eccf5dad9424b32cddb3706d22

module Legacy.Plutus.V2.Ledger.Tx(
    -- * Transactions
    Tx(..),
    inputs,
    referenceInputs,
    collateralInputs,
    outputs,
    spentOutputs,
    updateUtxoCollateral,
    validValuesTx,
    mintScripts,
    signatures,
    datumWitnesses,
    redeemers,
    lookupSignature,
    lookupDatum,
    lookupRedeemer,
    mint,
    fee,
    -- ** Stripped transactions
    TxStripped(..),
    strip,
    -- * Transaction outputs
    TxOutTx(..),
    -- txOutTxDatum,
    -- * Transaction inputs
    validRange,
    -- * Addresses
    Address
    ) where

import Codec.CBOR.Write qualified as Write
import Codec.Serialise.Class (Serialise, encode)
import Control.DeepSeq (NFData)
import Control.Lens (Lens', lens)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.ByteArray qualified as BA
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)

import PlutusTx.Lattice (BoundedMeetSemiLattice (top), MeetSemiLattice ((/\)))

import Legacy.Plutus.V1.Ledger.Crypto (PubKey, Signature)
import Plutus.V1.Ledger.Address (Address)

import Legacy.Plutus.V1.Ledger.Slot (SlotRange)
import Plutus.V1.Ledger.DCert (DCert)
import Plutus.V1.Ledger.Scripts (Datum, DatumHash, MintingPolicy, Redeemer)
import Plutus.V1.Ledger.Value (Value)
import Plutus.V1.Ledger.Value qualified as V

import Legacy.Plutus.V1.Ledger.Address ()
import Legacy.Plutus.V1.Ledger.Interval ()
import Legacy.Plutus.V1.Ledger.Orphans ()
import Legacy.Plutus.V1.Ledger.Scripts ()
import Legacy.Plutus.V1.Ledger.Value ()
import Plutus.V2.Ledger.Contexts (ScriptPurpose, TxOut (txOutValue))
import Plutus.V2.Ledger.Tx (OutputDatum, RedeemerPtr, Redeemers, ScriptTag, TxId, TxIn (txInRef), TxInType, TxOutRef)

{- Note [Serialisation and hashing]

We use cryptonite for generating hashes, which requires us to serialise values
to a strict ByteString (to implement `Data.ByteArray.ByteArrayAccess`).

Binary serialisation could be achieved via

1. The `binary` package
2. The `cbor` package

(1) is used in the cardano-sl repository, and (2) is used in the
`plutus-core` project in this repository.

In this module we use (2) because of the precedent. This means however that we
may generate different hashes for the same transactions compared to cardano-sl.
This might become a problem if/when we want to support "imports" of some real
blockchain state into the emulator.

However, it should be easy to change the serialisation mechanism later on,
especially because we only need one direction (to binary).

-}

deriving anyclass instance ToJSON OutputDatum
deriving anyclass instance FromJSON OutputDatum
deriving anyclass instance Serialise OutputDatum

deriving anyclass instance ToJSON TxOut
deriving anyclass instance FromJSON TxOut
deriving anyclass instance Serialise TxOut

deriving anyclass instance ToJSON TxInType
deriving anyclass instance FromJSON TxInType
deriving anyclass instance Serialise TxInType

deriving anyclass instance ToJSON TxId
deriving anyclass instance FromJSON TxId
deriving anyclass instance ToJSONKey TxId
deriving anyclass instance FromJSONKey TxId
deriving anyclass instance Serialise TxId

deriving anyclass instance ToJSON TxOutRef
deriving anyclass instance FromJSON TxOutRef
deriving anyclass instance ToJSONKey TxOutRef
deriving anyclass instance FromJSONKey TxOutRef
deriving anyclass instance Serialise TxOutRef

deriving anyclass instance ToJSON TxIn
deriving anyclass instance FromJSON TxIn
deriving anyclass instance Serialise TxIn

deriving anyclass instance ToJSON ScriptTag
deriving anyclass instance FromJSON ScriptTag
deriving anyclass instance Serialise ScriptTag

deriving anyclass instance ToJSON DCert
deriving anyclass instance FromJSON DCert
deriving anyclass instance Serialise DCert

deriving anyclass instance ToJSON ScriptPurpose
deriving anyclass instance FromJSON ScriptPurpose
deriving anyclass instance Serialise ScriptPurpose
deriving anyclass instance ToJSONKey ScriptPurpose
deriving anyclass instance FromJSONKey ScriptPurpose
deriving anyclass instance NFData ScriptPurpose
deriving anyclass instance Ord ScriptPurpose

deriving anyclass instance ToJSON RedeemerPtr
deriving anyclass instance FromJSON RedeemerPtr
deriving anyclass instance ToJSONKey RedeemerPtr
deriving anyclass instance FromJSONKey RedeemerPtr
deriving anyclass instance Serialise RedeemerPtr

-- | A transaction, including witnesses for its inputs.
data Tx = Tx {
    txInputs            :: Set.Set TxIn,
    -- ^ The inputs to this transaction.
    txReferenceInputs   :: Set.Set TxIn,
    -- ^ The reference inputs to this transaction.
    txCollateral        :: Set.Set TxIn,
    -- ^ The collateral inputs to cover the fees in case validation of the transaction fails.
    txOutputs           :: [TxOut],
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    txMint              :: !Value,
    -- ^ The 'Value' minted by this transaction.
    txFee               :: !Value,
    -- ^ The fee for this transaction.
    txValidRange        :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    txMintScripts       :: Set.Set MintingPolicy,
    -- ^ The scripts that must be run to check minting conditions.
    txSignatures        :: Map PubKey Signature,
    -- ^ Signatures of this transaction.
    txRedeemers         :: Redeemers,
    -- ^ Redeemers of the minting scripts.
    txSpendingRedeemers :: Map ScriptPurpose Redeemer,
    -- ^ Redeemers of the consumed script outputs.
    txData              :: Map DatumHash Datum
    -- ^ Datum objects recorded on this transaction.
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

instance Semigroup Tx where
    tx1 <> tx2 = Tx {
        txInputs = txInputs tx1 <> txInputs tx2,
        txReferenceInputs = txReferenceInputs tx1 <> txReferenceInputs tx2,
        txCollateral = txCollateral tx1 <> txCollateral tx2,
        txOutputs = txOutputs tx1 <> txOutputs tx2,
        txMint = txMint tx1 <> txMint tx2,
        txFee = txFee tx1 <> txFee tx2,
        txValidRange = txValidRange tx1 /\ txValidRange tx2,
        txMintScripts = txMintScripts tx1 <> txMintScripts tx2,
        txSignatures = txSignatures tx1 <> txSignatures tx2,
        txRedeemers = txRedeemers tx1 <> txRedeemers tx2,
        txSpendingRedeemers = txSpendingRedeemers tx1 <> txSpendingRedeemers tx2,
        txData = txData tx1 <> txData tx2
        }

instance Monoid Tx where
    mempty = Tx mempty mempty mempty mempty mempty mempty top mempty mempty mempty mempty mempty

instance BA.ByteArrayAccess Tx where
    length        = BA.length . Write.toStrictByteString . encode
    withByteArray = BA.withByteArray . Write.toStrictByteString . encode

-- | The inputs of a transaction.
inputs :: Lens' Tx (Set.Set TxIn)
inputs = lens g s where
    g = txInputs
    s tx i = tx { txInputs = i }

-- | The reference inputs of a transaction.
referenceInputs :: Lens' Tx (Set.Set TxIn)
referenceInputs = lens g s where
    g = txInputs
    s tx i = tx { txReferenceInputs = i }

-- | The collateral inputs of a transaction for paying fees when validating the transaction fails.
collateralInputs :: Lens' Tx (Set.Set TxIn)
collateralInputs = lens g s where
    g = txCollateral
    s tx i = tx { txCollateral = i }

-- | The outputs of a transaction.
outputs :: Lens' Tx [TxOut]
outputs = lens g s where
    g = txOutputs
    s tx o = tx { txOutputs = o }

-- | The validity range of a transaction.
validRange :: Lens' Tx SlotRange
validRange = lens g s where
    g = txValidRange
    s tx o = tx { txValidRange = o }

signatures :: Lens' Tx (Map PubKey Signature)
signatures = lens g s where
    g = txSignatures
    s tx sig = tx { txSignatures = sig }

fee :: Lens' Tx Value
fee = lens g s where
    g = txFee
    s tx v = tx { txFee = v }

mint :: Lens' Tx Value
mint = lens g s where
    g = txMint
    s tx v = tx { txMint = v }

mintScripts :: Lens' Tx (Set.Set MintingPolicy)
mintScripts = lens g s where
    g = txMintScripts
    s tx fs = tx { txMintScripts = fs }

redeemers :: Lens' Tx Redeemers
redeemers = lens g s where
    g = txRedeemers
    s tx reds = tx { txRedeemers = reds }

datumWitnesses :: Lens' Tx (Map DatumHash Datum)
datumWitnesses = lens g s where
    g = txData
    s tx dat = tx { txData = dat }

lookupSignature :: PubKey -> Tx -> Maybe Signature
lookupSignature s Tx{txSignatures} = Map.lookup s txSignatures

lookupDatum :: Tx -> DatumHash -> Maybe Datum
lookupDatum Tx{txData} h = Map.lookup h txData

lookupRedeemer :: Tx -> RedeemerPtr -> Maybe Redeemer
lookupRedeemer tx p = Map.lookup p (txRedeemers tx)

-- | Check that all values in a transaction are non-negative.
validValuesTx :: Tx -> Bool
validValuesTx Tx{..}
  = all (nonNegative . txOutValue) txOutputs  && nonNegative txFee
    where
      nonNegative i = V.geq i mempty

-- | A transaction without witnesses for its inputs.
data TxStripped = TxStripped {
    txStrippedInputs  :: Set.Set TxOutRef,
    -- ^ The inputs to this transaction, as transaction output references only.
    txStrippedOutputs :: [TxOut],
    -- ^ The outputs of this transation.
    txStrippedMint    :: !Value,
    -- ^ The 'Value' minted by this transaction.
    txStrippedFee     :: !Value
    -- ^ The fee for this transaction.
    } deriving (Show, Eq, Generic, Serialise)

strip :: Tx -> TxStripped
strip Tx{..} = TxStripped i txOutputs txMint txFee where
    i = Set.map txInRef txInputs

-- | A 'TxOut' along with the 'Tx' it comes from, which may have additional information e.g.
-- the full data script that goes with the 'TxOut'.
data TxOutTx = TxOutTx { txOutTxTx :: Tx, txOutTxOut :: TxOut }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON)

-- TODO: MELD: implement this properly
-- txOutTxDatum :: TxOutTx -> Maybe Datum
-- txOutTxDatum (TxOutTx tx out) = txOutDatum out >>= lookupDatum tx

-- | The transaction output references consumed by a transaction.
spentOutputs :: Tx -> Set.Set TxOutRef
spentOutputs = Set.map txInRef . txInputs

-- | Update a map of unspent transaction outputs and signatures
--   for a failed transaction using its collateral inputs.
updateUtxoCollateral :: Tx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxoCollateral tx unspent = unspent `Map.withoutKeys` (Set.map txInRef . txCollateral $ tx)
