module Pitboss.Sim.State.SpotHand where

import Pitboss.Blackjack.Hand (Hand)

data SpotHandPlayState
  = NormalPlay
  | HandFrozen
  | FrozenAfterSplitAce
  | Finalized
  deriving (Eq, Show)

data SpotHandState = SpotHandState
  { spotHandPlayState :: SpotHandPlayState,
    spotHand :: Hand
  }
  deriving (Eq, Show)
