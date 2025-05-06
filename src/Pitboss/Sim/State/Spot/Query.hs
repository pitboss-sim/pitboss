module Pitboss.Sim.State.Spot.Query where

import Pitboss.Sim.State.Spot (SpotHandIx, SpotState (..), Turn (..))
import Pitboss.Sim.State.SpotHand (SpotHandState)
import Pitboss.Sim.Types.FiniteMap (lookupFiniteMap)
import Pitboss.Sim.Types.Occupancy (Occupancy (..))
import Pitboss.Types.BoundedEnum (universe)

extractHand :: SpotState -> SpotHandIx -> Maybe SpotHandState
extractHand (SpotState handsMap _turn _) hid =
  case lookupFiniteMap hid handsMap of
    Just (Present handState) -> Just handState
    _ -> Nothing

getSelectedHand :: SpotState -> Maybe (SpotHandIx, SpotHandState)
getSelectedHand (SpotState handsMap turn' _) =
  case turn' of
    Playing hid ->
      case lookupFiniteMap hid handsMap of
        Just (Present hstate) -> Just (hid, hstate)
        _ -> Nothing
    NoHandSelected -> Nothing

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
