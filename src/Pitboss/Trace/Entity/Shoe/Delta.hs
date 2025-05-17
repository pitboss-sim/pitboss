{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.Trace.Entity.Shoe.Delta
  ( module Pitboss.Trace.Entity.Shoe.Delta.Attrs,
    module Pitboss.Trace.Entity.Shoe.Delta.Modes,
    module Pitboss.Trace.Entity.Shoe.Delta.Rels,
    ShoeDelta (..),
  )
where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Shoe
import Pitboss.Trace.Entity.Shoe.Delta.Attrs
import Pitboss.Trace.Entity.Shoe.Delta.Modes
import Pitboss.Trace.Entity.Shoe.Delta.Rels

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
