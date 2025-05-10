module Pitboss.World.State.Spot where

import Pitboss.World.State.Hand (HandState)
import Pitboss.World.State.Types.BoundedEnum (BoundedEnum)
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven
import Pitboss.World.State.Types.FiniteMap (FiniteMap, emptyFiniteMap, insertFiniteMap)
import Pitboss.World.State.Types.Occupancy (Occupancy (..))

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

data SpotDelta
  = SetOccupied Bool
  | SetSpotLabel String
  | SetHand HandIx HandState
  | ClearHand HandIx
  deriving (Eq, Show)

instance Clocked SpotState where
  tick = _tick
  setTick t ss = ss {_tick = t}

instance DeltaDriven SpotState SpotDelta where
  applyDelta d ss = case d of
    SetOccupied b -> ss {_spotOccupied = b}
    SetSpotLabel lbl -> ss {_spotLabel = lbl}
    SetHand ix hs ->
      ss {_spotHands = insertFiniteMap ix (Present hs) (_spotHands ss)}
    ClearHand ix ->
      ss {_spotHands = insertFiniteMap ix Absent (_spotHands ss)}

  previewDelta d ss = Just (applyDelta d ss)

  describeDelta d _ = case d of
    SetOccupied b -> "Set occupied to " ++ show b
    SetSpotLabel lbl -> "Set spot label to " ++ lbl
    SetHand ix _ -> "Set hand " ++ show ix
    ClearHand ix -> "Cleared hand " ++ show ix
