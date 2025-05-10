module Pitboss.World.State.Table where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.World.State.Offering (OfferingState)
import Pitboss.World.State.Round (RoundState)
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven
import Pitboss.World.Types.EntityRef (EntityRef)

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

data TableDelta
  = SetOffering (EntityRef OfferingState)
  | SetMinBet Chips
  | SetTableName String
  | StartRound (EntityRef RoundState)
  | EndRound
  deriving (Eq, Show, Generic)

instance ToJSON TableDelta

instance FromJSON TableDelta

instance Clocked TableState where
  tick = _tick
  setTick t ts = ts {_tick = t}

instance DeltaDriven TableState TableDelta where
  applyDelta delta ts = case delta of
    SetOffering ref -> ts {_offeringUsed = ref}
    SetMinBet amt -> ts {_minBet = amt}
    SetTableName name -> ts {_tableName = name}
    StartRound ref -> ts {_currentRound = Just ref}
    EndRound -> ts {_currentRound = Nothing}

  describeDelta delta _ = case delta of
    SetOffering _ -> "Set offering (frozen ref)"
    SetMinBet amt -> "Set minimum bet to " ++ show amt
    SetTableName name -> "Set table name to " ++ name
    StartRound rid -> "Started round " ++ show rid
    EndRound -> "Ended current round"

  previewDelta delta ts = Just (applyDelta delta ts)
