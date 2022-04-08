{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
-- Otherwise we get a complaint about the 'fromIntegral' call in the generated instance of 'Integral' for 'Ada'
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

-- Restored from https://github.com/input-output-hk/plutus/pull/4394/files#diff-7bbadbaf3c6c9e8bef6bbd8ccf10615ea3f1b5ee7668fb9031542fe89993d219

-- | Slots and slot ranges.
module Legacy.Plutus.V1.Ledger.Slot(
      Slot(..)
    , SlotRange
    ) where

import Codec.Serialise.Class (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Data
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Prelude qualified as Haskell
import Prettyprinter (Pretty (pretty), (<+>))


import PlutusTx qualified
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude

import Legacy.Plutus.V1.Ledger.Interval ()
import Plutus.V1.Ledger.Interval (Interval)

{- HLINT ignore "Redundant if" -}

-- | The slot number. This is a good proxy for time, since on the Cardano blockchain
-- slots pass at a constant rate.
newtype Slot = Slot { getSlot :: Integer }
    deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, Data)
    deriving anyclass (FromJSON, FromJSONKey, ToJSON, ToJSONKey, NFData)
    deriving newtype (AdditiveSemigroup, AdditiveMonoid, AdditiveGroup, Eq, Ord, Enum, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving newtype (Haskell.Num, Haskell.Enum, Haskell.Real, Haskell.Integral, Serialise, Hashable)

makeLift ''Slot

instance Pretty Slot where
    pretty (Slot i) = "Slot" <+> pretty i

-- | An 'Interval' of 'Slot's.
type SlotRange = Interval Slot
