module Pitboss.Trace.Entity.Round where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Shoe hiding (_tick)
import Pitboss.Trace.Timeline.EntityRef

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

instance Clocked RoundState where
  tick = _tick
  setTick t rs = rs {_tick = t}

data RoundDelta
  = SetShoeUsed (EntityRef ShoeState)
  | SetRoundNumber Int
  | SetActive Bool
  deriving (Eq, Show, Generic)

instance ToJSON RoundDelta

instance FromJSON RoundDelta
