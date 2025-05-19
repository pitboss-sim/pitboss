{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.Player.Entity where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.FSM.PlayerHand
import Pitboss.FSM.PlayerSpot
import Pitboss.FSM.PlayerTable
import Pitboss.Trace.Entity.Types.EntityId

mkPlayerEntityAttrs :: String -> Chips -> PlayerEntityAttrs
mkPlayerEntityAttrs = PlayerEntityAttrs

mkPlayerEntityModes :: SomePlayerTableFSM -> SomePlayerSpotFSM -> SomePlayerHandFSM -> PlayerEntityModes
mkPlayerEntityModes = PlayerEntityModes

mkPlayerEntityRels :: Maybe (ClockedRef PlayerEntityId) -> Maybe (ClockedRef TableEntityId) -> PlayerEntityRels
mkPlayerEntityRels = PlayerEntityRels

data PlayerEntityAttrs = PlayerEntityAttrs
    { _playerEntityAttrsPlayerName :: String
    , _playerEntityAttrsBankroll :: Chips
    }
    deriving (Eq, Show, Generic)

data PlayerEntityModes = PlayerEntityModes
    { _playerEntityModesPlayerTable :: SomePlayerTableFSM
    , _playerEntityModesPlayerSpot :: SomePlayerSpotFSM
    , _playerEntityModesPlayerHand :: SomePlayerHandFSM
    }
    deriving (Eq, Show, Generic)

data PlayerEntityRels = PlayerEntityRels
    { _playerEntityRelsClonedFrom :: Maybe (ClockedRef PlayerEntityId)
    , _playerEntityRelsSeatedAt :: Maybe (ClockedRef TableEntityId)
    }
    deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityAttrs

instance FromJSON PlayerEntityAttrs

instance ToJSON PlayerEntityModes

instance FromJSON PlayerEntityModes

instance ToJSON PlayerEntityRels

instance FromJSON PlayerEntityRels
