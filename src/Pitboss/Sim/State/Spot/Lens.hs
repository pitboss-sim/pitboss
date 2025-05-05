module Pitboss.Sim.State.Spot.Lens where

import Control.Lens (Lens', lens)
import Pitboss.Sim.State.Spot (SpotHandIx, SpotState (..), Turn)
import Pitboss.Sim.State.SpotHand (SpotHandState)
import Pitboss.Sim.Types.FiniteMap (FiniteMap)
import Pitboss.Sim.Types.Occupancy (Occupancy (..))

lensHands :: Lens' SpotState (FiniteMap SpotHandIx (Occupancy SpotHandState))
lensHands = lens hands (\s x -> s {hands = x})

lensTurn :: Lens' SpotState Turn
lensTurn = lens turn (\s x -> s {turn = x})
