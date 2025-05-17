{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Entity.PlayerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.FSM.PlayerHand
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkPlayerHand :: Meta (EntityRef PlayerHandId) -> SomePlayerHandFSM -> PlayerHandState -> PlayerHandRelations -> PlayerHand
mkPlayerHand = PlayerHand

mkPlayerHandState :: [Card] -> Chips -> Int -> Int -> PlayerHandState
mkPlayerHandState = PlayerHandState

mkPlayerHandRelations :: EntityRef PlayerSpotId -> EntityRef DealerRoundId -> EntityRef PlayerId -> PlayerHandRelations
mkPlayerHandRelations = PlayerHandRelations

data PlayerHand = PlayerHand
  { _playerHandMeta :: Meta (EntityRef PlayerHandId),
    _playerHandFsm :: SomePlayerHandFSM,
    _playerHandState :: PlayerHandState,
    _playerHandRels :: PlayerHandRelations
  }
  deriving (Eq, Show, Generic)

data PlayerHandState = PlayerHandState
  { _playerHandStateHandCards :: [Card],
    _playerHandStateOriginalBet :: Chips,
    _playerHandStateSplitDepth :: Int,
    _playerHandStateHandIx :: Int
  }
  deriving (Eq, Show, Generic)

data PlayerHandRelations = PlayerHandRelations
  { _playerHandRelsBelongsToPlayerSpot :: EntityRef PlayerSpotId,
    _playerHandRelsBelongsToRound :: EntityRef DealerRoundId,
    _playerHandRelsOwnedByPlayer :: EntityRef PlayerId
  }
  deriving (Eq, Show, Generic)

instance ToJSON PlayerHand

instance FromJSON PlayerHand

instance ToJSON PlayerHandState

instance FromJSON PlayerHandState

instance ToJSON PlayerHandRelations

instance FromJSON PlayerHandRelations
