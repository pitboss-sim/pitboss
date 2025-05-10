module Pitboss.Trace.Entity.Table where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Trace.Delta.Types.Clocked
import Pitboss.Trace.Entity.Offering hiding (_tick)
import Pitboss.Trace.Entity.Round hiding (_tick)
import Pitboss.Trace.Registry.EntityRef

mkTableState :: Tick -> String -> EntityRef OfferingState -> Chips -> TableState
mkTableState t name offering _minBet =
  TableState
    { _tick = t,
      _tableName = name,
      _currentRound = Nothing,
      _offeringUsed = offering,
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
