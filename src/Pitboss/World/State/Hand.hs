module Pitboss.World.State.Hand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.World.State.Types.Clocked (Clocked (..), Tick)
import Pitboss.World.State.Types.DeltaDriven (DeltaDriven (..))
import Pitboss.World.State.Types.Snapshot (StateSnapshot, defaultSnapshot)

data HandState = HandState
  { handTick :: Tick,
    handCards :: [Card],
    originalBet :: Chips,
    splitDepth :: Int,
    handIx :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON HandState

instance FromJSON HandState

data HandDelta
  = SetCards [Card]
  | SetOriginalBet Chips
  | SetSplitDepth Int
  | IncrementSplitDepth
  | SetHandIndex Int
  deriving (Eq, Show, Generic)

instance ToJSON HandDelta

instance FromJSON HandDelta

instance Clocked HandState where
  tick = handTick
  setTick t hs = hs {handTick = t}

instance DeltaDriven HandState HandDelta where
  applyDelta delta hs = case delta of
    SetCards cs -> hs {handCards = cs}
    SetOriginalBet bet -> hs {originalBet = bet}
    SetSplitDepth d -> hs {splitDepth = d}
    IncrementSplitDepth -> hs {splitDepth = splitDepth hs + 1}
    SetHandIndex ix -> hs {handIx = ix}

  previewDelta d hs = Just (applyDelta d hs)

  describeDelta d _ = case d of
    SetCards cs -> "Set cards: " ++ show cs
    SetOriginalBet b -> "Set original bet to " ++ show b
    SetSplitDepth d' -> "Set split depth to " ++ show d'
    IncrementSplitDepth -> "Incremented split depth"
    SetHandIndex ix -> "Set hand index to " ++ show ix

defaultHandState :: Tick -> [Card] -> Chips -> Int -> Int -> HandState
defaultHandState t cards bet depth ix =
  HandState
    { handTick = t,
      handCards = cards,
      originalBet = bet,
      splitDepth = depth,
      handIx = ix
    }

defaultHandSnapshot :: Tick -> [Card] -> Chips -> Int -> Int -> StateSnapshot HandState HandDelta
defaultHandSnapshot t cards bet depth ix =
  defaultSnapshot (defaultHandState t cards bet depth ix)
