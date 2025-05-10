module Pitboss.World.State.Spot where

import Pitboss.World.State.Hand (HandState)
import Pitboss.World.State.Types.BoundedEnum (BoundedEnum)
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven
import Pitboss.World.State.Types.FiniteMap (FiniteMap, emptyFiniteMap, insertFiniteMap)
import Pitboss.World.State.Types.Occupancy (Occupancy (..))
import Pitboss.World.State.Types.Snapshot (StateSnapshot, defaultSnapshot)

data HandIx = Hand1 | Hand2 | Hand3 | Hand4
  deriving (Eq, Ord, Show, Enum, Bounded)

instance BoundedEnum HandIx

data SpotState = SpotState
  { spotTick :: Tick,
    spotOccupied :: Bool,
    spotLabel :: String,
    spotHands :: FiniteMap HandIx (Occupancy HandState)
  }
  deriving (Eq, Show)

data SpotDelta
  = SetOccupied Bool
  | SetSpotLabel String
  | SetHand HandIx HandState
  | ClearHand HandIx
  deriving (Eq, Show)

instance Clocked SpotState where
  tick = spotTick
  setTick t ss = ss {spotTick = t}

instance DeltaDriven SpotState SpotDelta where
  applyDelta d ss = case d of
    SetOccupied b -> ss {spotOccupied = b}
    SetSpotLabel lbl -> ss {spotLabel = lbl}
    SetHand ix hs ->
      ss {spotHands = insertFiniteMap ix (Present hs) (spotHands ss)}
    ClearHand ix ->
      ss {spotHands = insertFiniteMap ix Absent (spotHands ss)}

  previewDelta d ss = Just (applyDelta d ss)

  describeDelta d _ = case d of
    SetOccupied b -> "Set occupied to " ++ show b
    SetSpotLabel lbl -> "Set spot label to " ++ lbl
    SetHand ix _ -> "Set hand " ++ show ix
    ClearHand ix -> "Cleared hand " ++ show ix

defaultSpotState :: Tick -> String -> SpotState
defaultSpotState t label =
  SpotState
    { spotTick = t,
      spotOccupied = False,
      spotLabel = label,
      spotHands = emptyFiniteMap Absent
    }

defaultSpotSnapshot :: Tick -> String -> StateSnapshot SpotState SpotDelta
defaultSpotSnapshot t label =
  defaultSnapshot (defaultSpotState t label)
