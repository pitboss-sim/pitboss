{-# LANGUAGE FlexibleContexts #-}

module Pitboss.Sim.State.Spot where

import Pitboss.Sim.State.SpotHand (SpotHandState)
import Pitboss.Sim.Types.FiniteMap
  ( FiniteMap,
    emptyFiniteMap,
    lookupFiniteMap,
  )
import Pitboss.Sim.Types.Occupancy (Occupancy (..))
import Pitboss.Types.BoundedEnum (BoundedEnum, universe)

data SpotHandIx = Hand1 | Hand2 | Hand3 | Hand4
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum SpotHandIx

data Turn
  = Playing SpotHandIx
  | NoHandSelected
  deriving (Eq, Show)

data PauseReason
  = PlayerRequestedBreak
  | CasinoIntervention
  | TechnicalError
  | ManualPause
  deriving (Eq, Show)

data SpotState = SpotState
  { hands :: FiniteMap SpotHandIx (Occupancy SpotHandState),
    turn :: Turn,
    pauseState :: Maybe PauseReason
  }
  deriving (Eq, Show)

emptySpot :: SpotState
emptySpot =
  SpotState
    { hands = emptyFiniteMap Absent,
      turn = NoHandSelected,
      pauseState = Nothing
    }

extractHand :: SpotState -> SpotHandIx -> Maybe SpotHandState
extractHand (SpotState handsMap _turn _pause) hid =
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
