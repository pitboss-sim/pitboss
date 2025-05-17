{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerHand.Delta
  ( module Pitboss.Trace.Entity.PlayerHand.Delta.Attrs,
    module Pitboss.Trace.Entity.PlayerHand.Delta.Modes,
    module Pitboss.Trace.Entity.PlayerHand.Delta.Rels,
    PlayerHandDelta (..),
  )
where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerHand
import Pitboss.Trace.Entity.PlayerHand.Delta.Attrs
import Pitboss.Trace.Entity.PlayerHand.Delta.Modes
import Pitboss.Trace.Entity.PlayerHand.Delta.Rels

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
      entity {_playerHandState = applyDelta d (_playerHandState entity)}
    PlayerHandRelationsDelta d ->
      entity {_playerHandRels = applyDelta d (_playerHandRels entity)}
    PlayerHandFSMDelta d ->
      applyDelta d entity

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    PlayerHandStateDelta sd -> describeDelta sd (_playerHandState entity)
    PlayerHandRelationsDelta rd -> describeDelta rd (_playerHandRels entity)
    PlayerHandFSMDelta _ -> "FSM replaced"

instance Reversible PlayerHandDelta where
  invert = \case
    PlayerHandStateDelta d -> PlayerHandStateDelta <$> invert d
    PlayerHandRelationsDelta d -> PlayerHandRelationsDelta <$> invert d
    PlayerHandFSMDelta d -> PlayerHandFSMDelta <$> invert d
