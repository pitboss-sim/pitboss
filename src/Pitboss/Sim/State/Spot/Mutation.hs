{-# LANGUAGE FlexibleContexts #-}

module Pitboss.Sim.State.Spot.Mutation where

import Pitboss.Sim.State.Spot (SpotState (..), Turn (..))
import Pitboss.Sim.Types.FiniteMap (lookupFiniteMap)
import Pitboss.Sim.Types.Occupancy (Occupancy (..))
import Pitboss.Types.BoundedEnum (universe)

advanceToNextHand :: SpotState -> SpotState
advanceToNextHand spot@(SpotState handsMap currentTurn _) =
  let activeHands = [hid | hid <- universe, lookupFiniteMap hid handsMap /= Just Absent]
   in case currentTurn of
        NoHandSelected ->
          case activeHands of
            (hid : _) -> spot {turn = Playing hid}
            [] -> spot {turn = NoHandSelected}
        Playing current ->
          let remaining = dropWhile (/= current) activeHands
           in case remaining of
                (_ : next : _) -> spot {turn = Playing next}
                _ -> spot {turn = NoHandSelected}
