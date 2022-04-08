{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Value
  ( module Export
  , adaValueOf
  , noAdaValue
  , adaOnlyValue
  , isAdaOnlyValue
  ) where

import Data.Fixed (Fixed (MkFixed), Micro)
import Plutus.V1.Ledger.Value as Export
import PlutusTx.Prelude (Bool, Eq (..), (-))

{-# INLINABLE noAdaValue #-}
-- | Value without any Ada.
noAdaValue :: Value -> Value
noAdaValue v = v - adaOnlyValue v

{-# INLINABLE adaOnlyValue #-}
-- | Value without any non-Ada.
adaOnlyValue :: Value -> Value
adaOnlyValue v = singleton adaSymbol adaToken (valueOf v adaSymbol adaToken)

{-# INLINABLE isAdaOnlyValue #-}
isAdaOnlyValue :: Value -> Bool
isAdaOnlyValue v = adaOnlyValue v == v

{-# INLINABLE adaValueOf #-}
-- | A 'Value' with the given amount of Ada (the currency unit).
--
--   @adaValueOf == toValue . adaOf@
--
adaValueOf :: Micro -> Value
adaValueOf (MkFixed x) = singleton adaSymbol adaToken x
