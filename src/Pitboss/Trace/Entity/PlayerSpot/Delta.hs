{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerSpot.Delta
  ( module Pitboss.Trace.Entity.PlayerSpot.Delta.Attrs,
    module Pitboss.Trace.Entity.PlayerSpot.Delta.Modes,
    module Pitboss.Trace.Entity.PlayerSpot.Delta.Rels,
    PlayerSpotEntityDelta (..),
  )
where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerSpot
import Pitboss.Trace.Entity.PlayerSpot.Delta.Attrs
import Pitboss.Trace.Entity.PlayerSpot.Delta.Modes
import Pitboss.Trace.Entity.PlayerSpot.Delta.Rels

data PlayerSpotEntityDelta
  = PlayerSpotEntityAttrsDelta PlayerSpotEntityAttrsDelta
  | PlayerSpotEntityModesDelta PlayerSpotEntityModesDelta
  | PlayerSpotEntityRelsDelta PlayerSpotEntityRelsDelta
  deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotEntityDelta

instance FromJSON PlayerSpotEntityDelta

instance Incremental PlayerSpotEntityDelta where
  type Target PlayerSpotEntityDelta = PlayerSpotEntity

  applyDelta delta entity = case delta of
    PlayerSpotEntityAttrsDelta sd ->
      entity {_playerSpotEntityAttrs = applyDelta sd (_playerSpotEntityAttrs entity)}
    PlayerSpotEntityModesDelta fd ->
      entity {_playerSpotEntityModes = applyDelta fd (_playerSpotEntityModes entity)}
    PlayerSpotEntityRelsDelta rd ->
      entity {_playerSpotEntityRels = applyDelta rd (_playerSpotEntityRels entity)}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta d entity = case d of
    PlayerSpotEntityAttrsDelta sd -> describeDelta sd (_playerSpotEntityAttrs entity)
    PlayerSpotEntityModesDelta fd -> describeDelta fd (_playerSpotEntityModes entity)
    PlayerSpotEntityRelsDelta rd -> describeDelta rd (_playerSpotEntityRels entity)

instance Reversible PlayerSpotEntityDelta where
  invert = \case
    PlayerSpotEntityAttrsDelta d -> PlayerSpotEntityAttrsDelta <$> invert d
    PlayerSpotEntityModesDelta d -> PlayerSpotEntityModesDelta <$> invert d
    PlayerSpotEntityRelsDelta d -> PlayerSpotEntityRelsDelta <$> invert d
