{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.PlayerSpot where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Delta.PlayerSpot.FSM
import Pitboss.Trace.Delta.PlayerSpot.Relation
import Pitboss.Trace.Delta.PlayerSpot.State
import Pitboss.Trace.Entity.PlayerSpot

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
      entity {_state = applyDelta sd (_state entity)}
    PlayerSpotRelationsDelta rd ->
      entity {_rels = applyDelta rd (_rels entity)}
    PlayerSpotFSMDelta fd -> applyDelta fd entity

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta d entity = case d of
    PlayerSpotStateDelta sd -> describeDelta sd (_state entity)
    PlayerSpotRelationsDelta rd -> describeDelta rd (_rels entity)
    PlayerSpotFSMDelta fd -> describeDelta fd entity

instance Reversible PlayerSpotDelta where
  invert = \case
    PlayerSpotStateDelta d -> PlayerSpotStateDelta <$> invert d
    PlayerSpotRelationsDelta d -> PlayerSpotRelationsDelta <$> invert d
    PlayerSpotFSMDelta d -> PlayerSpotFSMDelta <$> invert d
