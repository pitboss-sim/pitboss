module Pitboss.Sim.State.Shoe.Lens where

import Control.Lens (Lens', lens)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Sim.State.Shoe (ShoeState (..))

lensShoeCards :: Lens' ShoeState [Card]
lensShoeCards = lens shoeCards (\s x -> s {shoeCards = x})

lensShoeDiscarded :: Lens' ShoeState [Card]
lensShoeDiscarded = lens shoeDiscarded (\s x -> s {shoeDiscarded = x})
