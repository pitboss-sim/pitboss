module Pitboss.World.State.Table where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.World.State.Offering (OfferingState)
import Pitboss.World.State.Round (RoundState)
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven
import Pitboss.World.State.Types.Snapshot (EntityRef, StateSnapshot (..), defaultSnapshot)

data TableState = TableState
  { tableTick :: Tick,
    tableName :: String,
    currentRound :: Maybe (EntityRef RoundState),
    offeringUsed :: EntityRef OfferingState,
    minBet :: Chips
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
  tick = tableTick
  setTick t ts = ts {tableTick = t}

instance DeltaDriven TableState TableDelta where
  applyDelta delta ts = case delta of
    SetOffering ref -> ts {offeringUsed = ref}
    SetMinBet amt -> ts {minBet = amt}
    SetTableName name -> ts {tableName = name}
    StartRound ref -> ts {currentRound = Just ref}
    EndRound -> ts {currentRound = Nothing}

  describeDelta delta _ = case delta of
    SetOffering _ -> "Set offering (frozen ref)"
    SetMinBet amt -> "Set minimum bet to " ++ show amt
    SetTableName name -> "Set table name to " ++ name
    StartRound rid -> "Started round " ++ show rid
    EndRound -> "Ended current round"

  previewDelta delta ts = Just (applyDelta delta ts)

defaultTableState :: Tick -> String -> EntityRef OfferingState -> Chips -> TableState
defaultTableState t name offering minBet =
  TableState
    { tableTick = t,
      tableName = name,
      currentRound = Nothing,
      offeringUsed = offering,
      minBet = minBet
    }

defaultTableSnapshot :: Tick -> String -> EntityRef OfferingState -> Chips -> StateSnapshot TableState TableDelta
defaultTableSnapshot t name offering minBet =
  defaultSnapshot (defaultTableState t name offering minBet)
