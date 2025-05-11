{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Trace.Entity.Player where

import Pitboss.FSM.DealerRoundFSM
import Pitboss.FSM.PlayerTableFSM
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Capabilities.DeltaDriven
import Pitboss.Trace.Entity.Capabilities.Reversible
import Pitboss.Trace.Entity.Types.FiniteMap
import Pitboss.Trace.Entity.Types.FiniteMap.BoundedEnum
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy
import Pitboss.Trace.Timeline.Identifier

data PlayerState = PlayerState
  { fsm :: SomePlayerTableFSM,
    interrupt :: Maybe (InterruptReason, Tick),
    assignedTable :: Maybe TableId,
    playingSpots :: FiniteMap PlayerSpotIx (Occupancy SpotId)
  }

deriving instance Show PlayerState

deriving instance Eq PlayerState

data PlayerSpotIx = PlayerSpot1 | PlayerSpot2 | PlayerSpot3 | PlayerSpot4
  deriving (Eq, Show, Ord, Enum, Bounded)

instance BoundedEnum PlayerSpotIx

data PlayerDelta
  = ReplacePlayerFSM SomePlayerTableFSM SomePlayerTableFSM
  | ReplaceAssignedTable (Maybe TableId) (Maybe TableId)
  | ReplaceInterrupt (Maybe (InterruptReason, Tick)) (Maybe (InterruptReason, Tick))
  | ReplaceSpotClaim PlayerSpotIx (Occupancy SpotId) (Occupancy SpotId)
  deriving (Eq, Show)

instance Reversible PlayerDelta where
  invert = \case
    ReplacePlayerFSM old new -> Just $ ReplacePlayerFSM new old
    ReplaceAssignedTable old new -> Just $ ReplaceAssignedTable new old
    ReplaceInterrupt old new -> Just $ ReplaceInterrupt new old
    ReplaceSpotClaim ix old new -> Just $ ReplaceSpotClaim ix new old

instance DeltaDriven PlayerState PlayerDelta where
  applyDelta delta s = case delta of
    ReplacePlayerFSM _ new ->
      s {fsm = new}
    ReplaceAssignedTable _ new ->
      s {assignedTable = new}
    ReplaceInterrupt _ new ->
      s {interrupt = new}
    ReplaceSpotClaim ix _ new ->
      s {playingSpots = insertFiniteMap ix new (playingSpots s)}

  previewDelta delta s = Just (applyDelta delta s)

  describeDelta delta _ = case delta of
    ReplacePlayerFSM _ new ->
      "Set FSM to: " ++ show new
    ReplaceAssignedTable _ new ->
      "Set assigned table: " ++ maybe "None" show new
    ReplaceInterrupt _ new ->
      "Set interrupt: " ++ maybe "None" (const "Some") new
    ReplaceSpotClaim ix _ new ->
      "Set spot claim at " ++ show ix ++ " to " ++ show new
