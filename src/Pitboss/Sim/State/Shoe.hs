{-# LANGUAGE DeriveGeneric #-}

module Pitboss.Sim.State.Shoe where

import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)

data ShoeState = ShoeState
  { shoeCards :: [Card],
    shoeDiscarded :: [Card]
  }
  deriving (Eq, Show, Generic)

emptyShoe :: ShoeState
emptyShoe = ShoeState [] []
