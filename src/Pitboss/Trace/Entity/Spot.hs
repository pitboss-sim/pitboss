module Pitboss.Trace.Entity.Spot where

import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.PlayerHand hiding (_tick)
import Pitboss.Trace.Entity.Types.FiniteMap
import Pitboss.Trace.Entity.Types.FiniteMap.BoundedEnum
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy

mkSpotState :: Tick -> String -> SpotState
mkSpotState t label =
  SpotState
    { _tick = t,
      _spotOccupied = False,
      _spotLabel = label,
      _spotHands = emptyFiniteMap Absent
    }

data HandIx = Hand1 | Hand2 | Hand3 | Hand4
  deriving (Eq, Ord, Show, Enum, Bounded)

instance BoundedEnum HandIx

data SpotState = SpotState
  { _tick :: Tick,
    _spotOccupied :: Bool,
    _spotLabel :: String,
    _spotHands :: FiniteMap HandIx (Occupancy PlayerHandState)
  }
  deriving (Eq, Show)

instance Clocked SpotState where
  tick = _tick
  setTick t ss = ss {_tick = t}

data SpotDelta
  = SetOccupied Bool
  | SetSpotLabel String
  | SetHand HandIx PlayerHandState
  | ClearHand HandIx
  deriving (Eq, Show)
