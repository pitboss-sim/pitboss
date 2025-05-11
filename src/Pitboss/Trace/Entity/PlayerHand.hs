{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Entity.PlayerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.FSM.PlayerHandFSM.Existential
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Timeline.Identifier

mkPlayerHandState :: Tick -> [Card] -> Chips -> Int -> Int -> PlayerHandState
mkPlayerHandState t cards bet depth ix =
  PlayerHandState
    { _tick = t,
      _handCards = cards,
      _originalBet = bet,
      _splitDepth = depth,
      _handIx = ix
    }

data PlayerHandState = PlayerHandState
  { _tick :: Tick,
    _handCards :: [Card],
    _originalBet :: Chips,
    _splitDepth :: Int,
    _handIx :: Int
  }
  deriving (Eq, Show, Generic)

data PlayerHandEntity = PlayerHandEntity
  { _state :: PlayerHandState,
    _fsm :: SomePlayerHandFSM,
    _rels :: PlayerHandEntityRelations
  }

data PlayerHandEntityRelations = PlayerHandEntityRelations
  { _belongsToSpot :: SpotId,
    _belongsToRound :: RoundId,
    _ownedByPlayer :: PlayerId
  }

instance ToJSON PlayerHandState

instance FromJSON PlayerHandState

instance Clocked PlayerHandState where
  tick = _tick
  setTick t hs = hs {_tick = t}

data PlayerHandDelta
  = AddCard Card
  | RemoveCard Card
  | ReplaceCards [Card] [Card]
  | ReplacePlayerHandIndex Int Int
  | ReplaceSplitDepth Int Int
  deriving (Eq, Show, Generic)

instance ToJSON PlayerHandDelta

instance FromJSON PlayerHandDelta
