{-# LANGUAGE FlexibleContexts #-}

module Pitboss.Sim.State.Spot where

import Pitboss.Sim.State.SpotHand (SpotHandState)
import Pitboss.Sim.Types.FiniteMap
  ( FiniteMap,
    emptyFiniteMap,
  )
import Pitboss.Sim.Types.Occupancy (Occupancy (..))
import Pitboss.Types.BoundedEnum (BoundedEnum)

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
