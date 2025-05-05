module Pitboss.Sim.State.SpotHand.Lens where

import Control.Lens (Lens', lens)
import Pitboss.Blackjack.Hand (Hand)
import Pitboss.Sim.State.SpotHand (SpotHandPlayState (..), SpotHandState (..))

lensSpotHandPlayState :: Lens' SpotHandState SpotHandPlayState
lensSpotHandPlayState = lens spotHandPlayState (\s x -> s {spotHandPlayState = x})

lensSpotHand :: Lens' SpotHandState Hand
lensSpotHand = lens spotHand (\s x -> s {spotHand = x})
