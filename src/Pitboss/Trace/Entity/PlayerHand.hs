{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Entity.PlayerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.FSM.PlayerHandFSM
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.Identifier

mkPlayerHand :: Meta PlayerHandId -> SomePlayerHandFSM -> PlayerHandState -> PlayerHandRelations -> PlayerHand
mkPlayerHand = PlayerHand

mkPlayerHandState :: [Card] -> Chips -> Int -> Int -> PlayerHandState
mkPlayerHandState = PlayerHandState

mkPlayerHandRelations :: PlayerSpotId -> DealerRoundId -> PlayerId -> PlayerHandRelations
mkPlayerHandRelations = PlayerHandRelations

data PlayerHand = PlayerHand
  { _meta :: Meta PlayerHandId,
    _fsm :: SomePlayerHandFSM,
    _state :: PlayerHandState,
    _rels :: PlayerHandRelations
  }
  deriving (Eq, Show, Generic)

data PlayerHandState = PlayerHandState
  { _handCards :: [Card],
    _originalBet :: Chips,
    _splitDepth :: Int,
    _handIx :: Int
  }
  deriving (Eq, Show, Generic)

data PlayerHandRelations = PlayerHandRelations
  { _belongsToPlayerSpot :: PlayerSpotId,
    _belongsToRound :: DealerRoundId,
    _ownedByPlayer :: PlayerId
  }
  deriving (Eq, Show, Generic)

instance ToJSON PlayerHand

instance FromJSON PlayerHand

instance ToJSON PlayerHandState

instance FromJSON PlayerHandState

instance ToJSON PlayerHandRelations

instance FromJSON PlayerHandRelations
