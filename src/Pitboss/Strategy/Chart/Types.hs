module Pitboss.Strategy.Chart.Types where

import Data.Map.Strict
import Pitboss.Blackjack.Materia.Card (Rank)

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

data ChartEntry = ChartEntry
    { handPrefix :: HandPrefix
    , moves :: Map Rank MoveCode
    }
    deriving (Eq, Show)

type StrategyChart = [ChartEntry]
