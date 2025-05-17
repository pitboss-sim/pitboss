{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.Trace.Timeline where

import Data.Aeson
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities

newtype Timeline a where
  Timeline :: {unTimeline :: InsOrdHashMap Tick [a]} -> Timeline a
  deriving (Generic, Show, Eq)

instance Semigroup (Timeline a) where
  Timeline a <> Timeline b = Timeline (a <> b)

instance Monoid (Timeline a) where
  mempty = Timeline mempty

instance (ToJSON a) => ToJSON (Timeline a) where
  toJSON (Timeline m) = toJSON m

instance (FromJSON a) => FromJSON (Timeline a) where
  parseJSON v = Timeline <$> parseJSON v
