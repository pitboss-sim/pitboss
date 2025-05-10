{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Entity.DealerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Mechanics.Dealer.Types
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Timeline.Identifier

mkDealerHandState :: Tick -> [Card] -> Chips -> Int -> Int -> DealerHandState
mkDealerHandState t cards bet depth ix =
  DealerHandState
    { _tick = t,
      _handCards = cards,
      _originalBet = bet,
      _splitDepth = depth,
      _handIx = ix
    }

data DealerHandState = DealerHandState
  { _tick :: Tick,
    _handCards :: [Card],
    _originalBet :: Chips,
    _splitDepth :: Int,
    _handIx :: Int
  }
  deriving (Eq, Show, Generic)

data DealerHandEntity = DealerHandEntity
  { _state :: DealerHandState,
    _fsm :: DealerHandFSM,
    _rels :: DealerHandEntityRelations
  }

data DealerHandEntityRelations = DealerHandEntityRelations
  { _belongsToSpot :: SpotId,
    _belongsToRound :: RoundId,
    _ownedByDealer :: DealerId
  }

instance ToJSON DealerHandState

instance FromJSON DealerHandState

instance Clocked DealerHandState where
  tick = _tick
  setTick t hs = hs {_tick = t}

data DealerHandDelta
  = AddCard Card
  | RemoveCard Card
  | ReplaceCards [Card] [Card]
  | ReplaceDealerHandIndex Int Int
  | ReplaceSplitDepth Int Int
  deriving (Eq, Show, Generic)

instance ToJSON DealerHandDelta

instance FromJSON DealerHandDelta
