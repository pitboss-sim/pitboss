module Pitboss.World.State.Table where

import Pitboss.Blackjack.Chips (Chips)
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven
import Pitboss.World.Types.Identifier (OfferingId, RoundId)

data TableState = TableState
  { tableTick :: Tick,
    tableName :: String,
    currentRound :: Maybe RoundId,
    offeringUsed :: OfferingId,
    minBet :: Chips
  }
  deriving (Eq, Show)

data TableDelta
  = SetOffering OfferingId
  | SetMinBet Chips
  | SetTableName String
  | StartRound RoundId
  | EndRound
  deriving (Eq, Show)

instance Clocked TableState where
  tick = tableTick
  setTick t ts = ts {tableTick = t}

instance DeltaDriven TableState TableDelta where
  applyDelta delta ts = case delta of
    SetOffering oid -> ts {offeringUsed = oid}
    SetMinBet amt -> ts {minBet = amt}
    SetTableName name -> ts {tableName = name}
    StartRound rid -> ts {currentRound = Just rid}
    EndRound -> ts {currentRound = Nothing}

  describeDelta delta _ = case delta of
    SetOffering offering -> "Set offering to " ++ show offering
    SetMinBet amt -> "Set minimum bet to " ++ show amt
    SetTableName name -> "Set table name to " ++ show name
    StartRound rid -> "Started round " ++ show rid
    EndRound -> "Ended current round"

  previewDelta delta ts = Just (applyDelta delta ts)
