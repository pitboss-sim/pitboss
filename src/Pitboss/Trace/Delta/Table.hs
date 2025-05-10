module Pitboss.Trace.Delta.Table where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Trace.Delta.Types.Clocked
import Pitboss.Trace.Delta.Types.DeltaDriven
import Pitboss.Trace.Entity.Offering hiding (_tick)
import Pitboss.Trace.Entity.Round hiding (_tick)
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.Registry.EntityRef

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
