module Pitboss.Sim.State.SpotHand.Mutation where

import Control.Lens
import Pitboss.Sim.State.Spot
import Pitboss.Sim.State.Spot.Lens (lensHands)
import Pitboss.Sim.State.SpotHand (SpotHandPlayState (..), SpotHandState (..))
import Pitboss.Sim.Types.FiniteMap
import Pitboss.Sim.Types.Occupancy (Occupancy (..))

freezeOnly :: SpotHandIx -> SpotHandIx -> Occupancy SpotHandState -> Occupancy SpotHandState
freezeOnly target current (Present (SpotHandState NormalPlay h))
  | target == current = Present (SpotHandState HandFrozen h)
freezeOnly _ _ occ = occ

freezeSelectedPlayHand :: SpotHandIx -> SpotState -> SpotState
freezeSelectedPlayHand target spot =
  spot & lensHands %~ mapFiniteMapWithKey (freezeOnly target)
