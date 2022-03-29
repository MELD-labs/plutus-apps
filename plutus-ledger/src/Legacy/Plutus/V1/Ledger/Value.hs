-- This module restores all the instances removed by
-- https://github.com/input-output-hk/plutus/pull/4394/files#diff-42fc3c6ff95169aa5a2ef84502b9ed8750665c4acf8a357803b0cbd78cf8eb01

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Plutus.V1.Ledger.Value where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Legacy.Plutus.V1.Ledger.Orphans ()
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol, TokenName, Value)
import PlutusTx.AssocMap qualified as AssocMap

deriving anyclass instance (ToJSON a, ToJSON b) => ToJSON (AssocMap.Map a b)
deriving anyclass instance (FromJSON a, FromJSON b) => FromJSON (AssocMap.Map a b)
deriving anyclass instance (Serialise a, Serialise b) => Serialise (AssocMap.Map a b)

-- NOTE: by right, we should derive instances of ToJSON and FromJSON manually
-- See this part for more details:
-- https://github.com/input-output-hk/plutus/pull/4394/files#diff-42fc3c6ff95169aa5a2ef84502b9ed8750665c4acf8a357803b0cbd78cf8eb01L155-L177
deriving anyclass instance (ToJSON TokenName)
deriving anyclass instance (FromJSON TokenName)
deriving anyclass instance (Serialise TokenName)

-- NOTE: by right, we should derive instances of ToJSON and FromJSON manually
-- See this part for more details:
-- https://github.com/input-output-hk/plutus/pull/4394/files#diff-42fc3c6ff95169aa5a2ef84502b9ed8750665c4acf8a357803b0cbd78cf8eb01L86-L102
deriving anyclass instance (ToJSON CurrencySymbol)
deriving anyclass instance (FromJSON CurrencySymbol)
deriving anyclass instance (Serialise CurrencySymbol)

deriving anyclass instance ToJSON Value
deriving anyclass instance FromJSON Value
deriving anyclass instance Serialise Value

deriving anyclass instance ToJSON AssetClass
deriving anyclass instance FromJSON AssetClass
deriving anyclass instance Serialise AssetClass
