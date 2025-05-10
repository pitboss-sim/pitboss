module Pitboss.World.State.Round where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.World.State.Shoe (ShoeState)
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven
import Pitboss.World.Types.EntityRef (EntityRef)

mkRoundState :: Tick -> Int -> EntityRef ShoeState -> RoundState
mkRoundState t n shoe =
  RoundState
    { _tick = t,
      _roundNumber = n,
      _isActive = False,
      _shoeUsed = shoe
    }

data RoundState = RoundState
  { _tick :: Tick,
    _roundNumber :: Int,
    _isActive :: Bool,
    _shoeUsed :: EntityRef ShoeState
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
  tick = _tick
  setTick t rs = rs {_tick = t}

instance DeltaDriven RoundState RoundDelta where
  applyDelta d rs = case d of
    SetShoeUsed ref -> rs {_shoeUsed = ref}
    SetRoundNumber n -> rs {_roundNumber = n}
    SetActive b -> rs {_isActive = b}

  describeDelta :: RoundDelta -> entity -> String
  describeDelta d _ = case d of
    SetShoeUsed ref -> "Set shoe used: " ++ show ref
    SetRoundNumber n -> "Set round number to " ++ show n
    SetActive b -> "Set active status to " ++ show b

  previewDelta d rs = Just (applyDelta d rs)
