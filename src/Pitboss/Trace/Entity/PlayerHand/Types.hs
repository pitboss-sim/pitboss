{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.PlayerHand.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card
import Pitboss.Blackjack.Chips
import Pitboss.FSM.PlayerHand
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkPlayerHandEntityAttrs :: [Card] -> Chips -> Int -> Int -> PlayerHandEntityAttrs
mkPlayerHandEntityAttrs = PlayerHandEntityAttrs

mkPlayerHandEntityModes :: SomePlayerHandFSM -> PlayerHandEntityModes
mkPlayerHandEntityModes = PlayerHandEntityModes

mkPlayerHandEntityRels :: EntityRef PlayerSpotEntityId -> EntityRef DealerRoundEntityId -> EntityRef PlayerEntityId -> PlayerHandEntityRels
mkPlayerHandEntityRels = PlayerHandEntityRels

data PlayerHandEntityAttrs = PlayerHandEntityAttrs
    { _playerHandEntityAttrsHandCards :: [Card]
    , _playerHandEntityAttrsOriginalBet :: Chips
    , _playerHandEntityAttrsSplitDepth :: Int
    , _playerHandEntityAttrsHandIx :: Int
    }
    deriving (Eq, Show, Generic)

data PlayerHandEntityModes = PlayerHandEntityModes
    { _playerHandEntityFsm :: SomePlayerHandFSM
    }
    deriving (Eq, Show, Generic)

data PlayerHandEntityRels = PlayerHandEntityRels
    { _playerHandEntityRelsBelongsToPlayerSpot :: EntityRef PlayerSpotEntityId
    , _playerHandEntityRelsBelongsToDealerRound :: EntityRef DealerRoundEntityId
    , _playerHandEntityRelsOwnedByPlayer :: EntityRef PlayerEntityId
    }
    deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntityAttrs

instance FromJSON PlayerHandEntityAttrs

instance ToJSON PlayerHandEntityModes

instance FromJSON PlayerHandEntityModes

instance ToJSON PlayerHandEntityRels

instance FromJSON PlayerHandEntityRels
