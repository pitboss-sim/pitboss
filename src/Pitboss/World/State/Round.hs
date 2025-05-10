module Pitboss.World.State.Round where

import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven

data RoundState = RoundState
  { roundTick :: Tick,
    roundNumber :: Int,
    isActive :: Bool
  }
  deriving (Eq, Show)

data RoundDelta
  = SetRoundNumber Int
  | SetActive Bool
  deriving (Eq, Show)

instance Clocked RoundState where
  tick = roundTick
  setTick t rs = rs {roundTick = t}

instance DeltaDriven RoundState RoundDelta where
  applyDelta d rs = case d of
    SetRoundNumber n -> rs {roundNumber = n}
    SetActive b -> rs {isActive = b}

  describeDelta :: RoundDelta -> entity -> String
  describeDelta d _ = case d of
    SetRoundNumber n -> "Set round number to " ++ show n
    SetActive b -> "Set active status to " ++ show b

  previewDelta d rs = Just (applyDelta d rs)
