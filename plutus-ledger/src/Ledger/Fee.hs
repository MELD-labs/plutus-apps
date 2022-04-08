{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Configuring and calculating transaction fees in the emulator.
module Ledger.Fee(
  FeeConfig (..)
, calcFees
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Ledger.Index (minFee)
import Plutus.V1.Ledger.Value qualified as V

-- | Datatype to configure the fee in a transaction.
--
-- The fee for a transaction is typically: 'fcConstantFee + 'fcScriptsFeeFactor' *
-- <SIZE_DEPENDANT_SCRIPTS_FEE>.
data FeeConfig =
    FeeConfig
        { fcConstantFee      :: Integer -- ^ Constant fee per transaction in lovelace
        , fcScriptsFeeFactor :: Double  -- ^ Factor by which to multiply the size-dependent scripts fee
        }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Default FeeConfig where
  def = FeeConfig { fcConstantFee = V.valueOf (minFee mempty) V.adaSymbol V.adaToken
                  , fcScriptsFeeFactor = 1.0
                  }

calcFees :: FeeConfig
         -> Integer -- ^ Scripts fee in lovelace
         -> Integer -- ^ Fees in lovelace
calcFees FeeConfig { fcConstantFee , fcScriptsFeeFactor } scriptsFee =
     fcConstantFee + (floor $ fcScriptsFeeFactor * fromIntegral scriptsFee)
