module Pitboss.Trace.Entity.Player where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.Identifier

mkPlayer :: Meta PlayerId -> PlayerState -> PlayerRelations -> Player
mkPlayer = Player

mkPlayerState :: String -> Chips -> PlayerState
mkPlayerState = PlayerState

mkPlayerRelations :: Maybe PlayerId -> Maybe TableId -> PlayerRelations
mkPlayerRelations = PlayerRelations

data Player = Player
  { _meta :: Meta PlayerId,
    _state :: PlayerState,
    -- _fsm :: SomePlayerFSM, -- TBD
    _rels :: PlayerRelations
  }
  deriving (Show, Eq, Generic)

data PlayerState = PlayerState
  { _playerName :: String,
    _bankroll :: Chips
  }
  deriving (Eq, Show, Generic)

data PlayerRelations = PlayerRelations
  { _clonedFrom :: Maybe PlayerId,
    _seatedAt :: Maybe TableId
  }
  deriving (Eq, Show, Generic)

instance ToJSON Player

instance FromJSON Player

instance ToJSON PlayerState

instance FromJSON PlayerState

instance ToJSON PlayerRelations

instance FromJSON PlayerRelations
