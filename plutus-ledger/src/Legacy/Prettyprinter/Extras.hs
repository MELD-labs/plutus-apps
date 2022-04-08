-- Restored from https://github.com/input-output-hk/plutus/pull/4416/files#diff-89a4dfcd15185c0e9a4ccbbbc0aea1ab5a653d0f662c2f6708e554b67be7ec9d

{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Legacy.Prettyprinter.Extras(
    PrettyShow(..)
    , Pretty(..)
    , PrettyFoldable(..)
    , Tagged(Tagged)
    ) where

import Data.Foldable (Foldable (toList))
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Tagged
import GHC.TypeLits (KnownSymbol, symbolVal)
import Prettyprinter

-- | Newtype wrapper for deriving 'Pretty' via a 'Show' instance
newtype PrettyShow a = PrettyShow { unPrettyShow :: a }

instance {-# OVERLAPPING #-} Show a => Pretty (PrettyShow a) where
  pretty = viaShow . unPrettyShow

-- | Newtype wrapper for deriving 'Pretty' for a 'Foldable' container by
--   calling 'toList'.
newtype PrettyFoldable f a = PrettyFoldable { unPrettyFoldable :: f a }

instance {-# OVERLAPPING #-} (Foldable f, Pretty a) => Pretty (PrettyFoldable f a) where
  pretty = pretty . toList . unPrettyFoldable

instance {-# OVERLAPPING #-} (KnownSymbol a, Pretty b) => Pretty (Tagged a b) where
  pretty = prettyTagged

prettyTagged :: forall a b ann. (KnownSymbol a, Pretty b) => Tagged a b -> Doc ann
prettyTagged (Tagged b) = fromString (symbolVal (Proxy @a)) <+> pretty b
