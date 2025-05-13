module Pitboss.Trace.Entity.Player where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.EntityRegistry.Identifier

data PlayerEntity = PlayerEntity
  { _tick :: Tick,
    _state :: PlayerState,
    -- _fsm :: SomePlayerFSM, -- TBD
    _rels :: PlayerEntityRelations
  }
  deriving (Show, Eq, Generic)

data PlayerState = PlayerState
  { _playerName :: String,
    _bankroll :: Chips
  }
  deriving (Eq, Show, Generic)

data PlayerEntityRelations = PlayerEntityRelations
  { _clonedFrom :: Maybe PlayerId,
    _seatedAt :: Maybe TableId
  }
  deriving (Eq, Show, Generic)

data PlayerDelta
  = RenamePlayer String
  | SetBankroll Chips
  deriving (Eq, Show, Generic)

instance Clocked PlayerEntity where
  tick = _tick
  setTick t p = p {_tick = t}

instance ToJSON PlayerEntity

instance FromJSON PlayerEntity

instance ToJSON PlayerState

instance FromJSON PlayerState

instance ToJSON PlayerDelta

instance FromJSON PlayerDelta

instance ToJSON PlayerEntityRelations

instance FromJSON PlayerEntityRelations
