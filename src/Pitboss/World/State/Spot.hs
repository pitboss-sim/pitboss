module Pitboss.World.State.Spot where

import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven

data SpotState = SpotState
  { spotTick :: Tick,
    spotOccupied :: Bool,
    spotLabel :: String
  }
  deriving (Eq, Show)

data SpotDelta
  = SetOccupied Bool
  | SetSpotLabel String
  deriving (Eq, Show)

instance Clocked SpotState where
  tick = spotTick
  setTick t ss = ss {spotTick = t}

instance DeltaDriven SpotState SpotDelta where
  applyDelta d ss = case d of
    SetOccupied b -> ss {spotOccupied = b}
    SetSpotLabel lbl -> ss {spotLabel = lbl}

  describeDelta :: SpotDelta -> entity -> String
  describeDelta d _ = case d of
    SetOccupied b -> "Set occupied to " ++ show b
    SetSpotLabel lbl -> "Set spot label to " ++ show lbl

  previewDelta d ss = Just (applyDelta d ss)
