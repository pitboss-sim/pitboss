{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.PlayerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.FSM.PlayerHand
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkPlayerHandEntity :: Meta (EntityRef PlayerHandEntityId) -> PlayerHandEntityAttrs -> PlayerHandEntityModes -> PlayerHandEntityRels -> PlayerHandEntity
mkPlayerHandEntity = PlayerHandEntity

mkPlayerHandEntityAttrs :: [Card] -> Chips -> Int -> Int -> PlayerHandEntityAttrs
mkPlayerHandEntityAttrs = PlayerHandEntityAttrs

mkPlayerHandEntityRels :: EntityRef PlayerSpotEntityId -> EntityRef DealerRoundEntityId -> EntityRef PlayerEntityId -> PlayerHandEntityRels
mkPlayerHandEntityRels = PlayerHandEntityRels

data PlayerHandEntity = PlayerHandEntity
  { _playerHandEntityMeta :: Meta (EntityRef PlayerHandEntityId),
    _playerHandEntityAttrs :: PlayerHandEntityAttrs,
    _playerHandEntityModes :: PlayerHandEntityModes,
    _playerHandEntityRels :: PlayerHandEntityRels
  }
  deriving (Eq, Show, Generic)

data PlayerHandEntityAttrs = PlayerHandEntityAttrs
  { _playerHandEntityAttrsHandCards :: [Card],
    _playerHandEntityAttrsOriginalBet :: Chips,
    _playerHandEntityAttrsSplitDepth :: Int,
    _playerHandEntityAttrsHandIx :: Int
  }
  deriving (Eq, Show, Generic)

data PlayerHandEntityModes = PlayerHandEntityModes
  { _playerHandEntityFsm :: SomePlayerHandFSM
  }
  deriving (Eq, Show, Generic)

data PlayerHandEntityRels = PlayerHandEntityRels
  { _playerHandEntityRelsBelongsToPlayerSpot :: EntityRef PlayerSpotEntityId,
    _playerHandEntityRelsBelongsToDealerRound :: EntityRef DealerRoundEntityId,
    _playerHandEntityRelsOwnedByPlayer :: EntityRef PlayerEntityId
  }
  deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntity

instance FromJSON PlayerHandEntity

instance ToJSON PlayerHandEntityAttrs

instance FromJSON PlayerHandEntityAttrs

instance ToJSON PlayerHandEntityModes

instance FromJSON PlayerHandEntityModes

instance ToJSON PlayerHandEntityRels

instance FromJSON PlayerHandEntityRels
