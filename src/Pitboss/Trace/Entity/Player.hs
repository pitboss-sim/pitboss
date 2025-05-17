module Pitboss.Trace.Entity.Player where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkPlayer :: Meta (EntityRef PlayerId) -> PlayerState -> PlayerRelations -> Player
mkPlayer = Player

mkPlayerState :: String -> Chips -> PlayerState
mkPlayerState = PlayerState

mkPlayerRelations :: Maybe (EntityRef PlayerId) -> Maybe (EntityRef TableId) -> PlayerRelations
mkPlayerRelations = PlayerRelations

data Player = Player
  { _playerMeta :: Meta (EntityRef PlayerId),
    _playerState :: PlayerState,
    -- _playerFsm :: SomePlayerFSM -- TBD
    _playerRels :: PlayerRelations
  }
  deriving (Show, Eq, Generic)

data PlayerState = PlayerState
  { _playerStatePlayerName :: String,
    _playerStateBankroll :: Chips
  }
  deriving (Eq, Show, Generic)

data PlayerRelations = PlayerRelations
  { _playerRelsClonedFrom :: Maybe (EntityRef PlayerId),
    _playerRelsSeatedAt :: Maybe (EntityRef TableId)
  }
  deriving (Eq, Show, Generic)

instance ToJSON Player

instance FromJSON Player

instance ToJSON PlayerState

instance FromJSON PlayerState

instance ToJSON PlayerRelations

instance FromJSON PlayerRelations
