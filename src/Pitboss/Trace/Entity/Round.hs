module Pitboss.Trace.Entity.Round where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Types.Clocked
import Pitboss.Trace.Entity.Shoe hiding (_tick)
import Pitboss.Trace.Registry.EntityRef

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
