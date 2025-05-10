{-# LANGUAGE LambdaCase #-}

module Pitboss.World.State.Actor where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.World.State.Table (TableState)
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven
import Pitboss.World.Types.EntityRef (EntityRef)

mkPlayerActorState :: Tick -> String -> ActorState
mkPlayerActorState t name = PlayerActorState t (PlayerActor name)

mkDealerActorState :: Tick -> String -> Maybe (EntityRef TableState) -> ActorState
mkDealerActorState t name assigned =
  DealerActorState t (DealerActor name assigned)

data PlayerActor = PlayerActor
  { _playerName :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON PlayerActor

instance FromJSON PlayerActor

data DealerActor = DealerActor
  { _dealerName :: String,
    _assignedTable :: Maybe (EntityRef TableState)
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
      RenameActor name -> PlayerActorState t (p {_playerName = name})
      _ -> PlayerActorState t p
    DealerActorState t d -> case delta of
      RenameActor name -> DealerActorState t (d {_dealerName = name})
      AssignTable tid -> DealerActorState t (d {_assignedTable = Just tid})
      UnassignTable -> DealerActorState t (d {_assignedTable = Nothing})

  describeDelta :: ActorDelta -> entity -> String
  describeDelta d _ = case d of
    RenameActor name -> "Renamed actor to " ++ name
    AssignTable tid -> "Assigned to table " ++ show tid
    UnassignTable -> "Unassigned from table"

  previewDelta d s = Just (applyDelta d s)
