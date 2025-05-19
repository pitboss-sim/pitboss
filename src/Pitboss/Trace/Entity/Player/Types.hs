{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.Player.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.FSM.PlayerHand
import Pitboss.FSM.PlayerSpot
import Pitboss.FSM.PlayerTable
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkPlayerEntityAttrs :: String -> Chips -> PlayerEntityAttrs
mkPlayerEntityAttrs = PlayerEntityAttrs

mkPlayerEntityModes :: SomePlayerTableFSM -> SomePlayerSpotFSM -> SomePlayerHandFSM -> PlayerEntityModes
mkPlayerEntityModes = PlayerEntityModes

mkPlayerEntityRels :: Maybe (EntityRef PlayerEntityId) -> Maybe (EntityRef TableEntityId) -> PlayerEntityRels
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
    { _playerEntityRelsClonedFrom :: Maybe (EntityRef PlayerEntityId)
    , _playerEntityRelsSeatedAt :: Maybe (EntityRef TableEntityId)
    }
    deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityAttrs

instance FromJSON PlayerEntityAttrs

instance ToJSON PlayerEntityModes

instance FromJSON PlayerEntityModes

instance ToJSON PlayerEntityRels

instance FromJSON PlayerEntityRels
