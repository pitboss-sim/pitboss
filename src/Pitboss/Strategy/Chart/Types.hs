module Pitboss.Strategy.Chart.Types where

import Data.Map.Strict
import Pitboss.Blackjack.Materia.Card (Rank)
import Pitboss.Blackjack.Materia.Hand (HandKind (..))

-- Keep HandPrefix only for parsing/serialization
data HandPrefix
    = PA
    | PT
    | P Int
    | A Int
    | H Int
    deriving (Eq, Ord, Show)

data MoveCode
    = MoveHit
    | MoveStand
    | MoveDoubleOrHit
    | MoveDoubleOrStand
    | MoveSplit
    | MoveSplitOrHit
    | MoveSurrenderOrStand
    | MoveUndefined
    deriving (Eq, Show)

-- Strategy charts now use HandKind directly
data ChartEntry = ChartEntry
    { handKind :: HandKind
    , kindValue :: Maybe Int  -- for H/A/P types that need values
    , moves :: Map Rank MoveCode
    }
    deriving (Eq, Show)

type StrategyChart = [ChartEntry]

-- Conversion functions for parsing/display
handPrefixToKind :: HandPrefix -> (HandKind, Maybe Int)
handPrefixToKind PA = (PairHand, Just 1)  -- Aces
handPrefixToKind PT = (BlackjackHand, Nothing)
handPrefixToKind (P n) = (PairHand, Just n)
handPrefixToKind (A n) = (SoftHand, Just n)
handPrefixToKind (H n) = (HardHand, Just n)

kindToHandPrefix :: HandKind -> Maybe Int -> HandPrefix
kindToHandPrefix PairHand (Just 1) = PA
kindToHandPrefix BlackjackHand _ = PT
kindToHandPrefix PairHand (Just n) = P n
kindToHandPrefix SoftHand (Just n) = A n
kindToHandPrefix HardHand (Just n) = H n
kindToHandPrefix TwentyOneHand (Just n) = H n
kindToHandPrefix BustHand (Just n) = H n
kindToHandPrefix _ _ = H 0  -- fallback
