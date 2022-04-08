{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Plutus.V1.Ledger.Time where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON (parseJSON), ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Scientific (floatingOrInteger, scientific)
import Plutus.V1.Ledger.Time (POSIXTime (POSIXTime))

-- | Custom `FromJSON` instance which allows to parse a JSON number to a
-- 'POSIXTime' value. The parsed JSON value MUST be an 'Integer' or else the
-- parsing fails.
instance FromJSON POSIXTime where
  parseJSON v@(Aeson.Number n) =
      either (\_ -> prependFailure "parsing POSIXTime failed, " (typeMismatch "Integer" v))
             (return . POSIXTime)
             (floatingOrInteger n :: Either Double Integer)
  parseJSON invalid =
      prependFailure "parsing POSIXTime failed, " (typeMismatch "Number" invalid)

-- | Custom 'ToJSON' instance which allows to simply convert a 'POSIXTime'
-- value to a JSON number.
instance ToJSON POSIXTime where
  toJSON (POSIXTime n) = Aeson.Number $ scientific n 0

deriving anyclass instance Serialise POSIXTime
