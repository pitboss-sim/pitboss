module Pitboss.Trace.Entity.Spot where

import Pitboss.Trace.Delta.Types.Clocked
import Pitboss.Trace.Entity.Hand hiding (_tick)
import Pitboss.Trace.FiniteMap
import Pitboss.Trace.FiniteMap.BoundedEnum
import Pitboss.Trace.FiniteMap.Occupancy

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
    _spotHands :: FiniteMap HandIx (Occupancy HandState)
  }
  deriving (Eq, Show)

instance Clocked SpotState where
  tick = _tick
  setTick t ss = ss {_tick = t}
