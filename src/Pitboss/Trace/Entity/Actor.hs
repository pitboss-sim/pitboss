{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Trace.Entity.Actor where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.Registry.EntityRef

mkPlayerActorState :: Tick -> String -> ActorState
mkPlayerActorState t name = PlayerActorState t (PlayerActor name)

mkDealerActorState :: Tick -> String -> Maybe (EntityRef TableState) -> ActorState
mkDealerActorState t name assigned =
  DealerActorState t (DealerActor name assigned)

data PlayerActor = PlayerActor
  { _playerName :: String
  }
  deriving (Eq, Show, Generic)

data DealerActor = DealerActor
  { _dealerName :: String,
    _assignedTable :: Maybe (EntityRef TableState)
  }
  deriving (Eq, Show, Generic)

data ActorState
  = PlayerActorState Tick PlayerActor
  | DealerActorState Tick DealerActor
  deriving (Eq, Show, Generic)

data ActorDelta
  = RenameActor String
  | AssignTable (EntityRef TableState)
  | UnassignTable
  deriving (Eq, Show, Generic)

instance ToJSON PlayerActor

instance FromJSON PlayerActor

instance ToJSON DealerActor

instance FromJSON DealerActor

instance ToJSON ActorState

instance FromJSON ActorState

instance Clocked ActorState where
  tick = \case
    PlayerActorState t _ -> t
    DealerActorState t _ -> t

  setTick newTick = \case
    PlayerActorState _ p -> PlayerActorState newTick p
    DealerActorState _ d -> DealerActorState newTick d

instance ToJSON ActorDelta

instance FromJSON ActorDelta
