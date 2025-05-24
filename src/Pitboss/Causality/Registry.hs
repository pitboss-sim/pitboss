{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.Causality.Registry where

import Data.Aeson.Types
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Hashable (Hashable)
import Pitboss.Causality.Timeline
import Pitboss.Causality.Types.Core

newtype Registry (k :: EntityKind) delta = Registry
    { unRegistry :: InsOrdHashMap (EntityIdFor k) (Timeline k delta)
    }

deriving instance (Eq (EntityIdFor k), Eq (Timeline k a)) => Eq (Registry k a)
deriving instance (Show a, Show (EntityIdFor k), Show (Timeline k a)) => Show (Registry k a)

instance (ToJSON (Timeline k delta), ToJSONKey (EntityIdFor k)) => ToJSON (Registry k delta) where
    toJSON (Registry m) = toJSON m

instance (FromJSON (Timeline k delta), FromJSONKey (EntityIdFor k), Ord (EntityIdFor k), Hashable (EntityIdFor k)) => FromJSON (Registry k delta) where
    parseJSON v = Registry <$> parseJSON v

instance (Hashable (EntityIdFor k)) => Monoid (Registry k delta) where
    mempty = Registry mempty

instance (Hashable (EntityIdFor k)) => Semigroup (Registry k delta) where
    Registry a <> Registry b = Registry (a <> b)
