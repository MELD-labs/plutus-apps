
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Restored from https://github.com/input-output-hk/plutus/pull/4394/files#diff-6a187ebe6f7d7f1d33dd8f15bf3ea03dc1be064c3f48596ef910b78f56c169e6

module Legacy.Plutus.V1.Ledger.Interval where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Plutus.V1.Ledger.Interval (Extended, LowerBound, UpperBound, Interval)

deriving anyclass instance (ToJSON a) => ToJSON (Extended a)
deriving anyclass instance (FromJSON a) => FromJSON (Extended a)
deriving anyclass instance (Serialise a) => Serialise (Extended a)

deriving anyclass instance (ToJSON a) => ToJSON (LowerBound a)
deriving anyclass instance (FromJSON a) => FromJSON (LowerBound a)
deriving anyclass instance (Serialise a) => Serialise (LowerBound a)

deriving anyclass instance (ToJSON a) => ToJSON (UpperBound a)
deriving anyclass instance (FromJSON a) => FromJSON (UpperBound a)
deriving anyclass instance (Serialise a) => Serialise (UpperBound a)

deriving anyclass instance (ToJSON a) => ToJSON (Interval a)
deriving anyclass instance (FromJSON a) => FromJSON (Interval a)
deriving anyclass instance (Serialise a) => Serialise (Interval a)
