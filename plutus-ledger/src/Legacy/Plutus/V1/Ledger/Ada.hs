-- Restored from https://github.com/input-output-hk/plutus/pull/4394/files#diff-b5f82503fb0d30da7de57c0508f8711c42a289de8d2e48bdad64bdb6ffa7cade

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
-- Otherwise we get a complaint about the 'fromIntegral' call in the generated instance of 'Integral' for 'Ada'
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- | Functions for working with 'Ada' in Template Haskell.
module Legacy.Plutus.V1.Ledger.Ada(
      Ada (..)
    , getAda
    , adaSymbol
    , adaToken
    -- * Constructors
    , fromValue
    , toValue
    , lovelaceOf
    , adaOf
    , lovelaceValueOf
    , adaValueOf
    -- * Num operations
    , divide
    -- * Etc.
    , isZero
    ) where

import Prelude qualified as Haskell

import Data.Fixed

import Codec.Serialise.Class (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Value (CurrencySymbol (..), TokenName (..), Value)
import Plutus.V1.Ledger.Value qualified as TH
import PlutusTx qualified
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude hiding (divide)
import PlutusTx.Prelude qualified as P

{-# INLINABLE adaSymbol #-}
-- | The 'CurrencySymbol' of the 'Ada' currency.
adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol emptyByteString

{-# INLINABLE adaToken #-}
-- | The 'TokenName' of the 'Ada' currency.
adaToken :: TokenName
adaToken = TokenName emptyByteString

-- | ADA, the special currency on the Cardano blockchain. The unit of Ada is Lovelace, and
--   1M Lovelace is one Ada.
--   See note [Currencies] in 'Ledger.Validation.Value.TH'.
newtype Ada = Lovelace { getLovelace :: Integer }
    deriving (Haskell.Enum)
    deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Eq, Ord, Haskell.Num, AdditiveSemigroup, AdditiveMonoid, AdditiveGroup, MultiplicativeSemigroup, MultiplicativeMonoid, Haskell.Integral, Haskell.Real, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

instance Haskell.Semigroup Ada where
    Lovelace a1 <> Lovelace a2 = Lovelace (a1 + a2)

instance Semigroup Ada where
    Lovelace a1 <> Lovelace a2 = Lovelace (a1 + a2)

instance Haskell.Monoid Ada where
    mempty = Lovelace 0

instance Monoid Ada where
    mempty = Lovelace 0

makeLift ''Ada

{-# INLINABLE getAda #-}
-- | Get the amount of Ada (the unit of the currency Ada) in this 'Ada' value.
getAda :: Ada -> Micro
getAda (Lovelace i) = MkFixed i

{-# INLINABLE toValue #-}
-- | Create a 'Value' containing only the given 'Ada'.
toValue :: Ada -> Value
toValue (Lovelace i) = TH.singleton adaSymbol adaToken i

{-# INLINABLE fromValue #-}
-- | Get the 'Ada' in the given 'Value'.
fromValue :: Value -> Ada
fromValue v = Lovelace (TH.valueOf v adaSymbol adaToken)

{-# INLINABLE lovelaceOf #-}
-- | Create 'Ada' representing the given quantity of Lovelace (the unit of the currency Ada).
lovelaceOf :: Integer -> Ada
lovelaceOf = Lovelace

{-# INLINABLE adaOf #-}
-- | Create 'Ada' representing the given quantity of Ada (1M Lovelace).
adaOf :: Micro -> Ada
adaOf (MkFixed x) = Lovelace x

{-# INLINABLE lovelaceValueOf #-}
-- | A 'Value' with the given amount of Lovelace (the currency unit).
--
--   @lovelaceValueOf == toValue . lovelaceOf@
--
lovelaceValueOf :: Integer -> Value
lovelaceValueOf = TH.singleton adaSymbol adaToken

{-# INLINABLE adaValueOf #-}
-- | A 'Value' with the given amount of Ada (the currency unit).
--
--   @adaValueOf == toValue . adaOf@
--
adaValueOf :: Micro -> Value
adaValueOf (MkFixed x) = TH.singleton adaSymbol adaToken x

{-# INLINABLE divide #-}
-- | Divide one 'Ada' value by another.
divide :: Ada -> Ada -> Ada
divide (Lovelace a) (Lovelace b) = Lovelace (P.divide a b)

{-# INLINABLE isZero #-}
-- | Check whether an 'Ada' value is zero.
isZero :: Ada -> Bool
isZero (Lovelace i) = i == 0
