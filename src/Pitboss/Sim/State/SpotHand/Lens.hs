module Pitboss.Sim.State.SpotHand.Lens where

import Control.Lens (Lens', lens)
import Pitboss.Blackjack.FSM.Hand (SomeHandFSM)
import Pitboss.Blackjack.Hand (Hand)
import Pitboss.Sim.State.SpotHand (SpotHandState (..))

lensSpotHandFSM :: Lens' SpotHandState SomeHandFSM
lensSpotHandFSM = lens spotHandFSM (\s x -> s {spotHandFSM = x})

lensHand :: Lens' SpotHandState Hand
lensHand = lens hand (\s x -> s {hand = x})
