module Pitboss.Trace.Delta.Spot where

import Pitboss.Trace.Delta.Types.DeltaDriven
import Pitboss.Trace.Entity.Hand hiding (_tick)
import Pitboss.Trace.Entity.Spot hiding (_tick)
import Pitboss.Trace.FiniteMap
import Pitboss.Trace.FiniteMap.Occupancy

data SpotDelta
  = SetOccupied Bool
  | SetSpotLabel String
  | SetHand HandIx HandState
  | ClearHand HandIx
  deriving (Eq, Show)

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
