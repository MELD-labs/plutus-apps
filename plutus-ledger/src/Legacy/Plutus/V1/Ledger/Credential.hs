-- Restored from https://github.com/input-output-hk/plutus/pull/4394/files#diff-326e5af148877a7c2e91dccd619ee201089aded5b49ac1eb44acb83b46ec3555

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Plutus.V1.Ledger.Credential where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Legacy.Plutus.V1.Ledger.Crypto ()
import Legacy.Plutus.V1.Ledger.Scripts ()
import Plutus.V1.Ledger.Credential (Credential, StakingCredential)

deriving anyclass instance ToJSON Credential
deriving anyclass instance FromJSON Credential
deriving anyclass instance Serialise Credential
deriving anyclass instance Hashable Credential

deriving anyclass instance ToJSON StakingCredential
deriving anyclass instance FromJSON StakingCredential
deriving anyclass instance Serialise StakingCredential
deriving anyclass instance Hashable StakingCredential
