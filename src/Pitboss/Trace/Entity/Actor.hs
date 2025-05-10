{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Trace.Entity.Actor where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Types.Clocked
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

instance Clocked ActorState where
  tick = \case
    PlayerActorState t _ -> t
    DealerActorState t _ -> t

  setTick newTick = \case
    PlayerActorState _ p -> PlayerActorState newTick p
    DealerActorState _ d -> DealerActorState newTick d
