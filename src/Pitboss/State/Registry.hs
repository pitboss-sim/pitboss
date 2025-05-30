{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.State.Registry (
    Registry (..),
) where

-- timeline utilities

import Data.Aeson
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Pitboss.State.Timeline
import Pitboss.State.Types.Core

-- delta timeline per type

newtype Registry (k :: EntityKind) delta = Registry
    { unRegistry :: InsOrdHashMap (EntityId k) (Timeline k delta)
    }

deriving instance (Eq a, Eq (Timeline k a)) => Eq (Registry k a)
deriving instance (Show a, Show (Timeline k a)) => Show (Registry k a)

instance (ToJSON (Timeline k delta)) => ToJSON (Registry k delta) where
    toJSON (Registry m) = toJSON m

instance (FromJSON (Timeline k delta)) => FromJSON (Registry k delta) where
    parseJSON v = Registry <$> parseJSON v

instance Monoid (Registry k delta) where
    mempty = Registry mempty

instance Semigroup (Registry k delta) where
    Registry a <> Registry b = Registry (a <> b)
