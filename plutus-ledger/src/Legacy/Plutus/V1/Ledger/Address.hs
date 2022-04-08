
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Restored from https://github.com/input-output-hk/plutus/pull/4394/files#diff-e7b68bf3cfeba0365a3434748c9bfff209782ac40aa6abcd731c37d6b99cac6a

module Legacy.Plutus.V1.Ledger.Address where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Credential ()
import Plutus.V1.Ledger.Crypto ()
import Plutus.V1.Ledger.Scripts ()

import Legacy.Plutus.V1.Ledger.Credential ()

deriving anyclass instance ToJSON Address
deriving anyclass instance FromJSON Address
deriving anyclass instance ToJSONKey Address
deriving anyclass instance FromJSONKey Address
deriving anyclass instance Serialise Address
deriving anyclass instance Hashable Address
