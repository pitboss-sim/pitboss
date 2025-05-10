module Pitboss.Trace.Entity.Table where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Offering hiding (_tick)
import Pitboss.Trace.Entity.Round hiding (_tick)
import Pitboss.Trace.Timeline.EntityRef

mkTableState :: Tick -> String -> EntityRef OfferingState -> Chips -> TableState
mkTableState t name offering _minBet =
  TableState
    { _tick = t,
      _offeringUsed = offering,
      _tableName = name,
      _currentRound = Nothing,
      _minBet = _minBet
    }

data TableState = TableState
  { _tick :: Tick,
    _tableName :: String,
    _currentRound :: Maybe (EntityRef RoundState),
    _offeringUsed :: EntityRef OfferingState,
    _minBet :: Chips
  }
  deriving (Eq, Show, Generic)

instance ToJSON TableState

instance FromJSON TableState

instance Clocked TableState where
  tick = _tick
  setTick t ts = ts {_tick = t}

data TableDelta
  = SetOffering (EntityRef OfferingState)
  | SetMinBet Chips
  | SetTableName String
  | StartRound (EntityRef RoundState)
  | EndRound
  deriving (Eq, Show, Generic)

instance ToJSON TableDelta

instance FromJSON TableDelta
