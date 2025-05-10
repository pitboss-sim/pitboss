{-# LANGUAGE LambdaCase #-}

module Pitboss.World.State.Actor where

import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven
import Pitboss.World.Types.Identifier (TableId)

data PlayerActor = PlayerActor
  { playerName :: String
  }
  deriving (Eq, Show)

data DealerActor = DealerActor
  { dealerName :: String,
    assignedTable :: Maybe TableId
  }
  deriving (Eq, Show)

data ActorState
  = PlayerActorState Tick PlayerActor
  | DealerActorState Tick DealerActor
  deriving (Eq, Show)

data ActorDelta
  = RenameActor String
  | AssignTable TableId
  | UnassignTable
  deriving (Eq, Show)

instance Clocked ActorState where
  tick = \case
    PlayerActorState t _ -> t
    DealerActorState t _ -> t

  setTick newTick = \case
    PlayerActorState _ p -> PlayerActorState newTick p
    DealerActorState _ d -> DealerActorState newTick d

instance DeltaDriven ActorState ActorDelta where
  applyDelta delta = \case
    PlayerActorState t p -> case delta of
      RenameActor name -> PlayerActorState t (p {playerName = name})
      _ -> PlayerActorState t p
    DealerActorState t d -> case delta of
      RenameActor name -> DealerActorState t (d {dealerName = name})
      AssignTable tid -> DealerActorState t (d {assignedTable = Just tid})
      UnassignTable -> DealerActorState t (d {assignedTable = Nothing})

  describeDelta :: ActorDelta -> entity -> String
  describeDelta d _ = case d of
    RenameActor name -> "Renamed actor to " ++ name
    AssignTable tid -> "Assigned to table " ++ show tid
    UnassignTable -> "Unassigned from table"

  previewDelta d s = Just (applyDelta d s)
