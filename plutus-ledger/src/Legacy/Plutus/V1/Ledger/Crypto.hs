-- Restored from https://github.com/input-output-hk/plutus/pull/4394/files#diff-6f56cc8130a9f3785f83d058df8e40396cf00cd62c25024a544db25900277eb4

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fobject-code #-}


module Legacy.Plutus.V1.Ledger.Crypto(
    PubKey(..)
    , PrivateKey(..)
    , Signature(..)
    ) where

import Codec.Serialise.Class (Serialise)
import Control.DeepSeq (NFData)
import Control.Newtype.Generics (Newtype)
import Data.Aeson (FromJSON (parseJSON), FromJSONKey, FromJSONKeyFunction (FromJSONKeyValue), ToJSON (toJSON),
                   ToJSONKey, ToJSONKeyFunction (ToJSONKeyValue), genericParseJSON, genericToJSON, (.:))
import Data.Aeson qualified as JSON
import Data.Hashable (Hashable)
import Data.String
import GHC.Generics (Generic)
import Legacy.Data.Aeson.Extras qualified as JSON
import Legacy.Plutus.V1.Ledger.Orphans ()
import Plutus.V1.Ledger.Bytes (LedgerBytes (..))
import PlutusTx qualified
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter

import Legacy.Plutus.V1.Ledger.Bytes ()
import Plutus.V1.Ledger.Crypto (PubKeyHash)

-- | A cryptographic public key.
newtype PubKey = PubKey { getPubKey :: LedgerBytes }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (Newtype, ToJSON, FromJSON, NFData)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving IsString via LedgerBytes
    deriving (Show, Pretty) via LedgerBytes
makeLift ''PubKey

instance ToJSONKey PubKey where
  toJSONKey = ToJSONKeyValue (genericToJSON JSON.defaultOptions) JSON.toEncoding

instance FromJSONKey PubKey where
  fromJSONKey = FromJSONKeyValue (genericParseJSON JSON.defaultOptions)

-- | A cryptographic private key.
newtype PrivateKey = PrivateKey { getPrivateKey :: LedgerBytes }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Newtype, ToJSONKey, FromJSONKey)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving (Show, Pretty) via LedgerBytes
    deriving Hashable via PlutusTx.BuiltinByteString

makeLift ''PrivateKey

-- | A message with a cryptographic signature.
newtype Signature = Signature { getSignature :: PlutusTx.BuiltinByteString }
    deriving stock (Eq, Ord, Generic)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, Serialise, NFData, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving (Show, Pretty) via LedgerBytes

instance ToJSON Signature where
  toJSON signature =
    JSON.object
      [ ( "getSignature"
        , JSON.String .
          JSON.encodeByteString .
          PlutusTx.fromBuiltin .
          getSignature $
          signature)
      ]

instance FromJSON Signature where
  parseJSON =
    JSON.withObject "Signature" $ \object -> do
      raw <- object .: "getSignature"
      bytes <- JSON.decodeByteString raw
      pure . Signature $ PlutusTx.toBuiltin bytes

makeLift ''Signature

deriving anyclass instance ToJSON PubKeyHash
deriving anyclass instance FromJSON PubKeyHash
deriving anyclass instance ToJSONKey PubKeyHash
deriving anyclass instance FromJSONKey PubKeyHash
deriving anyclass instance Serialise PubKeyHash
deriving anyclass instance Hashable PubKeyHash
