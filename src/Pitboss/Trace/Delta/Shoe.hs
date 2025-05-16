{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.Shoe where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.Shoe

data ShoeDelta = NoShoeDelta
  deriving (Eq, Show, Generic)

instance ToJSON ShoeDelta

instance FromJSON ShoeDelta

instance Incremental ShoeDelta where
  type Entity ShoeDelta = Shoe

  applyDelta _ s = s
  describeDelta _ _ = "No-op delta for Shoe"
  previewDelta _ = Just

instance Reversible ShoeDelta where
  invert = \case
    NoShoeDelta -> Right NoShoeDelta
