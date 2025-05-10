module Pitboss.World.State.Table where

import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven
import Pitboss.World.Types.Identifier (RoundId)

data TableState = TableState
  { tableTick :: Tick,
    tableName :: String,
    currentRound :: Maybe RoundId
  }
  deriving (Eq, Show)

data TableDelta
  = SetTableName String
  | StartRound RoundId
  | EndRound
  deriving (Eq, Show)

instance Clocked TableState where
  tick = tableTick
  setTick t ts = ts {tableTick = t}

instance DeltaDriven TableState TableDelta where
  applyDelta delta ts = case delta of
    SetTableName name -> ts {tableName = name}
    StartRound rid -> ts {currentRound = Just rid}
    EndRound -> ts {currentRound = Nothing}

  describeDelta delta _ = case delta of
    SetTableName name -> "Set table name to " ++ name
    StartRound rid -> "Started round " ++ show rid
    EndRound -> "Ended current round"

  previewDelta delta ts = Just (applyDelta delta ts)
