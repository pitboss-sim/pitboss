module Pitboss.World.State.Round where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.World.State.Shoe (ShoeState)
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven
import Pitboss.World.State.Types.Snapshot (EntityRef, StateSnapshot (..), defaultSnapshot)

data RoundState = RoundState
  { roundTick :: Tick,
    roundNumber :: Int,
    isActive :: Bool,
    shoeUsed :: EntityRef ShoeState
  }
  deriving (Eq, Show, Generic)

instance ToJSON RoundState

instance FromJSON RoundState

data RoundDelta
  = SetShoeUsed (EntityRef ShoeState)
  | SetRoundNumber Int
  | SetActive Bool
  deriving (Eq, Show, Generic)

instance ToJSON RoundDelta

instance FromJSON RoundDelta

instance Clocked RoundState where
  tick = roundTick
  setTick t rs = rs {roundTick = t}

instance DeltaDriven RoundState RoundDelta where
  applyDelta d rs = case d of
    SetShoeUsed ref -> rs {shoeUsed = ref}
    SetRoundNumber n -> rs {roundNumber = n}
    SetActive b -> rs {isActive = b}

  describeDelta :: RoundDelta -> entity -> String
  describeDelta d _ = case d of
    SetShoeUsed ref -> "Set shoe used: " ++ show ref
    SetRoundNumber n -> "Set round number to " ++ show n
    SetActive b -> "Set active status to " ++ show b

  previewDelta d rs = Just (applyDelta d rs)

defaultRoundState :: Tick -> Int -> EntityRef ShoeState -> RoundState
defaultRoundState t n shoe =
  RoundState
    { roundTick = t,
      roundNumber = n,
      isActive = False,
      shoeUsed = shoe
    }

defaultRoundSnapshot :: Tick -> Int -> EntityRef ShoeState -> StateSnapshot RoundState RoundDelta
defaultRoundSnapshot t n shoe =
  defaultSnapshot (defaultRoundState t n shoe)
