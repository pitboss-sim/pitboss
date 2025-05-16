{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.PlayerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Delta.PlayerHand.FSM
import Pitboss.Trace.Delta.PlayerHand.Relation
import Pitboss.Trace.Delta.PlayerHand.State
import Pitboss.Trace.Entity.PlayerHand

data PlayerHandDelta
  = PlayerHandStateDelta PlayerHandStateDelta
  | PlayerHandRelationsDelta PlayerHandRelationsDelta
  | PlayerHandFSMDelta PlayerHandFSMDelta
  deriving (Eq, Show, Generic)

instance ToJSON PlayerHandDelta

instance FromJSON PlayerHandDelta

instance Incremental PlayerHandDelta where
  type Entity PlayerHandDelta = PlayerHand

  applyDelta delta entity = case delta of
    PlayerHandStateDelta d ->
      entity {_state = applyDelta d (_state entity)}
    PlayerHandRelationsDelta d ->
      entity {_rels = applyDelta d (_rels entity)}
    PlayerHandFSMDelta d ->
      applyDelta d entity

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    PlayerHandStateDelta sd -> describeDelta sd (_state entity)
    PlayerHandRelationsDelta rd -> describeDelta rd (_rels entity)
    PlayerHandFSMDelta _ -> "FSM replaced"

instance Reversible PlayerHandDelta where
  invert = \case
    PlayerHandStateDelta d -> PlayerHandStateDelta <$> invert d
    PlayerHandRelationsDelta d -> PlayerHandRelationsDelta <$> invert d
    PlayerHandFSMDelta d -> PlayerHandFSMDelta <$> invert d
