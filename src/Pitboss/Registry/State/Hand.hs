module Pitboss.Registry.State.Hand where

import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)

data HandState = HandState
  { handCards :: [Card],
    originalBet :: Chips,
    splitDepth :: Int,
    handIx :: Int
  }
  deriving (Eq, Show)
