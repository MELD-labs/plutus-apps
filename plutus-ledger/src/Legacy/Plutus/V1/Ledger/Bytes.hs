-- Restored from https://github.com/input-output-hk/plutus/pull/4394/files#diff-9a627697816037d31138a596c751e3f7fbbdace9077f37ea115db0713633d52b

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Legacy.Plutus.V1.Ledger.Bytes where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Legacy.Data.Aeson.Extras as JSON (decodeByteString, encodeByteString)
import Plutus.V1.Ledger.Bytes (bytes, fromBytes, LedgerBytes)

instance ToJSON LedgerBytes where
  toJSON = JSON.String . JSON.encodeByteString . bytes

instance FromJSON LedgerBytes where
  parseJSON v = fromBytes <$> JSON.decodeByteString v

deriving anyclass instance Serialise LedgerBytes
deriving anyclass instance JSON.ToJSONKey LedgerBytes
deriving anyclass instance JSON.FromJSONKey LedgerBytes
