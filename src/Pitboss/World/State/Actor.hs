{-# LANGUAGE LambdaCase #-}

module Pitboss.World.State.Actor where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.World.State.Table (TableState)
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven
import Pitboss.World.State.Types.Snapshot (EntityRef, StateSnapshot, defaultSnapshot)

data PlayerActor = PlayerActor
  { playerName :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON PlayerActor

instance FromJSON PlayerActor

data DealerActor = DealerActor
  { dealerName :: String,
    assignedTable :: Maybe (EntityRef TableState)
  }
  deriving (Eq, Show, Generic)

instance ToJSON DealerActor

instance FromJSON DealerActor

data ActorState
  = PlayerActorState Tick PlayerActor
  | DealerActorState Tick DealerActor
  deriving (Eq, Show, Generic)

instance ToJSON ActorState

instance FromJSON ActorState

data ActorDelta
  = RenameActor String
  | AssignTable (EntityRef TableState)
  | UnassignTable
  deriving (Eq, Show, Generic)

instance ToJSON ActorDelta

instance FromJSON ActorDelta

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

defaultPlayerActorState :: Tick -> String -> ActorState
defaultPlayerActorState t name = PlayerActorState t (PlayerActor name)

defaultDealerActorState :: Tick -> String -> Maybe (EntityRef TableState) -> ActorState
defaultDealerActorState t name assigned =
  DealerActorState t (DealerActor name assigned)

defaultPlayerSnapshot :: Tick -> String -> StateSnapshot ActorState ActorDelta
defaultPlayerSnapshot t name = defaultSnapshot (defaultPlayerActorState t name)

defaultDealerSnapshot :: Tick -> String -> Maybe (EntityRef TableState) -> StateSnapshot ActorState ActorDelta
defaultDealerSnapshot t name assigned =
  defaultSnapshot (defaultDealerActorState t name assigned)
