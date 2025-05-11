module Pitboss.Trace.Entity.Shoe where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Trace.Entity.Capabilities.Clocked

mkShoeState :: Tick -> [Card] -> Maybe Int -> ShoeState
mkShoeState t cards cut =
  ShoeState
    { _tick = t,
      _cardsRemaining = cards,
      _cutPoint = cut
    }

data ShoeState = ShoeState
  { _tick :: Tick,
    _cardsRemaining :: [Card],
    _cutPoint :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON ShoeState

instance FromJSON ShoeState

instance Clocked ShoeState where
  tick = _tick
  setTick t ss = ss {_tick = t}

data ShoeDelta
  = DrawCard Card
  | SetCutPoint (Maybe Int)
  | RefillShoe [Card]
  deriving (Eq, Show, Generic)

instance ToJSON ShoeDelta

instance FromJSON ShoeDelta
