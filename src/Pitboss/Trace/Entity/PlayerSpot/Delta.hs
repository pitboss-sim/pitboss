{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerSpot.Delta
  ( module Pitboss.Trace.Entity.PlayerSpot.Delta.Attrs,
    module Pitboss.Trace.Entity.PlayerSpot.Delta.Modes,
    module Pitboss.Trace.Entity.PlayerSpot.Delta.Rels,
    PlayerSpotDelta (..),
  )
where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerSpot
import Pitboss.Trace.Entity.PlayerSpot.Delta.Attrs
import Pitboss.Trace.Entity.PlayerSpot.Delta.Modes
import Pitboss.Trace.Entity.PlayerSpot.Delta.Rels

data PlayerSpotDelta
  = PlayerSpotStateDelta PlayerSpotStateDelta
  | PlayerSpotRelationsDelta PlayerSpotRelationsDelta
  | PlayerSpotFSMDelta PlayerSpotFSMDelta
  deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotDelta

instance FromJSON PlayerSpotDelta

instance Incremental PlayerSpotDelta where
  type Entity PlayerSpotDelta = PlayerSpot

  applyDelta delta entity = case delta of
    PlayerSpotStateDelta sd ->
      entity {_playerSpotState = applyDelta sd (_playerSpotState entity)}
    PlayerSpotRelationsDelta rd ->
      entity {_playerSpotRels = applyDelta rd (_playerSpotRels entity)}
    PlayerSpotFSMDelta fd -> applyDelta fd entity

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta d entity = case d of
    PlayerSpotStateDelta sd -> describeDelta sd (_playerSpotState entity)
    PlayerSpotRelationsDelta rd -> describeDelta rd (_playerSpotRels entity)
    PlayerSpotFSMDelta fd -> describeDelta fd entity

instance Reversible PlayerSpotDelta where
  invert = \case
    PlayerSpotStateDelta d -> PlayerSpotStateDelta <$> invert d
    PlayerSpotRelationsDelta d -> PlayerSpotRelationsDelta <$> invert d
    PlayerSpotFSMDelta d -> PlayerSpotFSMDelta <$> invert d
