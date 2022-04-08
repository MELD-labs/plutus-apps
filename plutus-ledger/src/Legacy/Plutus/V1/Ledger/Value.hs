-- This module restores all the instances removed by
-- https://github.com/input-output-hk/plutus/pull/4394/files#diff-42fc3c6ff95169aa5a2ef84502b9ed8750665c4acf8a357803b0cbd78cf8eb01
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Plutus.V1.Ledger.Value where

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON (toJSON), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as E
import Legacy.Data.Aeson.Extras (decodeByteString, encodeByteString, tryDecode)
import Legacy.Plutus.V1.Ledger.Orphans ()
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol (CurrencySymbol, unCurrencySymbol), TokenName (TokenName),
                               Value, tokenName)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as PlutusTx

fromText :: Text -> TokenName
fromText = tokenName . E.encodeUtf8

fromTokenName :: (BS.ByteString -> r) -> (Text -> r) -> TokenName -> r
fromTokenName handleBytestring handleText (TokenName bs) = either
  (\_ -> handleBytestring $ PlutusTx.fromBuiltin bs)
  handleText
  (E.decodeUtf8' (PlutusTx.fromBuiltin bs))

asBase16 :: BS.ByteString -> Text
asBase16 bs = Text.concat ["0x", encodeByteString bs]

-- Orphan instances for 'Map' to make this work
instance (ToJSON v, ToJSON k) => ToJSON (AssocMap.Map k v) where
    toJSON = Aeson.toJSON . AssocMap.toList

instance (FromJSON v, FromJSON k) => FromJSON (AssocMap.Map k v) where
    parseJSON v = AssocMap.fromList <$> Aeson.parseJSON v

deriving anyclass instance (Serialise a, Serialise b) => Serialise (AssocMap.Map a b)

{- note [Roundtripping token names]
How to properly roundtrip a token name that is not valid UTF-8 through PureScript
without a big rewrite of the API?
We prefix it with a zero byte so we can recognize it when we get a bytestring value back,
and we serialize it base16 encoded, with 0x in front so it will look as a hex string.
(Browsers don't render the zero byte.)
-}
instance ToJSON TokenName where
  toJSON =
    Aeson.object . pure . (,) (Key.fromString "unTokenName") . Aeson.toJSON
      . fromTokenName
        (\bs -> Text.cons '\NUL' (asBase16 bs))
        (\t -> case Text.take 1 t of "\NUL" -> Text.concat ["\NUL\NUL", t]; _ -> t)

instance FromJSON TokenName where
  parseJSON =
    Aeson.withObject "TokenName" $ \object -> do
      raw <- object .: "unTokenName"
      fromJSONText raw
   where
    fromJSONText t = case Text.take 3 t of
      "\NUL0x"       -> either fail (pure . tokenName) . tryDecode . Text.drop 3 $ t
      "\NUL\NUL\NUL" -> pure . fromText . Text.drop 2 $ t
      _              -> pure . fromText $ t

deriving anyclass instance (Serialise TokenName)

instance ToJSON CurrencySymbol where
  toJSON c =
    Aeson.object
      [
        ( "unCurrencySymbol"
        , Aeson.String
            . encodeByteString
            . PlutusTx.fromBuiltin
            . unCurrencySymbol
            $ c
        )
      ]

instance FromJSON CurrencySymbol where
  parseJSON =
    Aeson.withObject "CurrencySymbol" $ \object -> do
      raw <- object .: "unCurrencySymbol"
      bytes <- decodeByteString raw
      pure $ CurrencySymbol $ PlutusTx.toBuiltin bytes

deriving anyclass instance (Serialise CurrencySymbol)

deriving anyclass instance ToJSON Value
deriving anyclass instance FromJSON Value
deriving anyclass instance Serialise Value

deriving anyclass instance ToJSON AssetClass
deriving anyclass instance FromJSON AssetClass
deriving anyclass instance Serialise AssetClass
