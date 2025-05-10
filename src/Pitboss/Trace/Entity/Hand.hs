{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Entity.Hand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Trace.Entity.Capabilities.Clocked

mkHandState :: Tick -> [Card] -> Chips -> Int -> Int -> HandState
mkHandState t cards bet depth ix =
  HandState
    { _tick = t,
      _handCards = cards,
      _originalBet = bet,
      _splitDepth = depth,
      _handIx = ix
    }

data HandState = HandState
  { _tick :: Tick,
    _handCards :: [Card],
    _originalBet :: Chips,
    _splitDepth :: Int,
    _handIx :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON HandState

instance FromJSON HandState

instance Clocked HandState where
  tick = _tick
  setTick t hs = hs {_tick = t}

data HandDelta
  = AddCard Card
  | RemoveCard Card
  | ReplaceCards [Card] [Card]
  | ReplaceHandIndex Int Int
  | ReplaceSplitDepth Int Int
  deriving (Eq, Show, Generic)

instance ToJSON HandDelta

instance FromJSON HandDelta
