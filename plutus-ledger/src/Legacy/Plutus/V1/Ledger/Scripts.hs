-- Restore all the instances removed by
-- https://github.com/input-output-hk/plutus/pull/4394/files#diff-cb4fae72ab44c8b348c3d0f7dc80d29cc4f3c63805870ab6836ce435caece150

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Plutus.V1.Ledger.Scripts where

import Codec.Serialise (Serialise (encode))
import Codec.Serialise.Class (Serialise (decode))
import Codec.Serialise.Decoding (decodeBytes)
import Data.Aeson (FromJSON (parseJSON), FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson.Types (ToJSON (toJSON))
import Data.Aeson.Types qualified as Aeson
import Data.Hashable (Hashable)
import Flat qualified
import GHC.Generics (Generic)
import Legacy.Data.Aeson.Extras (decodeSerialise, encodeSerialise)
import Legacy.Plutus.V1.Ledger.Orphans ()

import Plutus.V1.Ledger.Scripts (Datum, DatumHash, MintingPolicy, MintingPolicyHash, Redeemer, RedeemerHash,
                                 Script (Script), ScriptError, ScriptHash, StakeValidator, StakeValidatorHash,
                                 Validator, ValidatorHash)

import Plutus.V2.Ledger.Api (BuiltinData (BuiltinData), builtinDataToData, dataToBuiltinData)
import PlutusCore.Data qualified

newtype JSONViaSerialise a = JSONViaSerialise a

instance Serialise a => ToJSON (JSONViaSerialise a) where
  toJSON (JSONViaSerialise a) = Aeson.String $ encodeSerialise a

instance Serialise a => FromJSON (JSONViaSerialise a) where
  parseJSON v = JSONViaSerialise <$> decodeSerialise v

newtype SerialiseViaFlat a = SerialiseViaFlat a

instance Flat.Flat a => Serialise (SerialiseViaFlat a) where
  encode (SerialiseViaFlat a) = encode $ Flat.flat a
  decode = do
    bs <- decodeBytes
    case Flat.unflat bs of
      Left err -> fail (show err)
      Right v  -> return (SerialiseViaFlat v)

deriving via (JSONViaSerialise PlutusCore.Data.Data) instance ToJSON PlutusCore.Data.Data
deriving via (JSONViaSerialise PlutusCore.Data.Data) instance FromJSON PlutusCore.Data.Data

instance ToJSON BuiltinData where
  toJSON = toJSON . builtinDataToData

instance FromJSON BuiltinData where
  parseJSON json = fmap dataToBuiltinData $ parseJSON json

deriving stock instance Generic BuiltinData
deriving anyclass instance Serialise BuiltinData

deriving anyclass instance ToJSON ValidatorHash
deriving anyclass instance FromJSON ValidatorHash
deriving anyclass instance ToJSONKey ValidatorHash
deriving anyclass instance FromJSONKey ValidatorHash
deriving anyclass instance Serialise ValidatorHash
deriving anyclass instance Hashable ValidatorHash

deriving anyclass instance ToJSON ScriptHash
deriving anyclass instance FromJSON ScriptHash
deriving anyclass instance ToJSONKey ScriptHash
deriving anyclass instance FromJSONKey ScriptHash
deriving anyclass instance Serialise ScriptHash
deriving anyclass instance Hashable ScriptHash

deriving anyclass instance ToJSON DatumHash
deriving anyclass instance FromJSON DatumHash
deriving anyclass instance ToJSONKey DatumHash
deriving anyclass instance FromJSONKey DatumHash
deriving anyclass instance Serialise DatumHash
deriving anyclass instance Hashable DatumHash

deriving anyclass instance ToJSON Datum
deriving anyclass instance FromJSON Datum
deriving anyclass instance ToJSONKey Datum
deriving anyclass instance FromJSONKey Datum
deriving anyclass instance Serialise Datum

deriving anyclass instance ToJSON RedeemerHash
deriving anyclass instance FromJSON RedeemerHash
deriving anyclass instance ToJSONKey RedeemerHash
deriving anyclass instance FromJSONKey RedeemerHash
deriving anyclass instance Serialise RedeemerHash

deriving anyclass instance ToJSON Redeemer
deriving anyclass instance FromJSON Redeemer
deriving anyclass instance ToJSONKey Redeemer
deriving anyclass instance FromJSONKey Redeemer
deriving anyclass instance Serialise Redeemer

deriving anyclass instance ToJSON MintingPolicy
deriving anyclass instance FromJSON MintingPolicy
deriving anyclass instance ToJSONKey MintingPolicy
deriving anyclass instance FromJSONKey MintingPolicy

deriving anyclass instance ToJSON MintingPolicyHash
deriving anyclass instance FromJSON MintingPolicyHash
deriving anyclass instance ToJSONKey MintingPolicyHash
deriving anyclass instance FromJSONKey MintingPolicyHash

deriving anyclass instance ToJSON StakeValidatorHash
deriving anyclass instance FromJSON StakeValidatorHash
deriving anyclass instance ToJSONKey StakeValidatorHash
deriving anyclass instance FromJSONKey StakeValidatorHash

deriving anyclass instance ToJSON StakeValidator
deriving anyclass instance FromJSON StakeValidator
deriving anyclass instance ToJSONKey StakeValidator
deriving anyclass instance FromJSONKey StakeValidator

deriving anyclass instance ToJSON ScriptError
deriving anyclass instance FromJSON ScriptError

instance ToJSON Script where
  -- See note [JSON instances for Script]
  toJSON (Script p) = Aeson.String $ encodeSerialise (SerialiseViaFlat p)

instance FromJSON Script where
  -- See note [JSON instances for Script]
  parseJSON v = do
    (SerialiseViaFlat p) <- decodeSerialise v
    return $ Script p

deriving anyclass instance ToJSON Validator
deriving anyclass instance FromJSON Validator
deriving anyclass instance ToJSONKey Validator
deriving anyclass instance FromJSONKey Validator
