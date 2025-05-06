module Pitboss.Sim.State.Shoe.Mutation where

import Pitboss.Blackjack.Card (Card)
import Pitboss.Sim.State.Shoe (ShoeState (..))

discardCard :: Card -> ShoeState -> ShoeState
discardCard c (ShoeState cs ds) = ShoeState cs (c : ds)
