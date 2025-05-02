{-# LANGUAGE LambdaCase #-}

module Pitboss.Strategy.Chart.Interpret where

import Data.Map.Strict qualified as Map
import Pitboss.Blackjack.Card (Rank (..))
import Pitboss.Blackjack.Hand.Category (HandCategory (..), HandRanks (..))
import Pitboss.Strategy.Chart.Types
import Pitboss.Strategy.Types

-- | Lookup a decision from a parsed StrategyChart.
lookupDecision :: StrategyChart -> HandCategory -> Rank -> Maybe Decision
lookupDecision chart hc upcard =
    let lookupMove :: HandPrefix -> Maybe MoveCode
        lookupMove prefix = do
            entry <- Map.lookup prefix chartMap
            Map.lookup upcard (moves entry)

        chartMap = Map.fromList [(handPrefix e, e) | e <- chart]
     in fmap toDecision $
            case hc of
                Empty -> lookupMove (H 0)
                Partial r -> lookupMove (H (rankValue r))
                Blackjack -> lookupMove PT
                TwentyOne -> lookupMove (H 21)
                Pair r -> lookupMove (P (rankValue r))
                Soft2 r -> lookupMove (A (rankValue r))
                SoftN (HandRanks rs) -> lookupMove (A (11 + sum (map rankValue rs)))
                Hard2 r1 r2 -> lookupMove (H (rankValue r1 + rankValue r2))
                HardN (HandRanks rs) -> lookupMove (H (sum (map rankValue rs)))

-- | Convert a MoveCode into a structured Decision.
toDecision :: MoveCode -> Decision
toDecision = \case
    MoveHit -> Always Hit
    MoveStand -> Always Stand
    MoveDoubleOrHit -> Prefer Double (Else Hit)
    MoveDoubleOrStand -> Prefer Double (Else Stand)
    MoveSplit -> Always Split
    MoveSplitOrHit -> Prefer Split (Else Hit)
    MoveSurrenderOrStand -> Prefer Surrender (Else Stand)
    MoveUndefined -> Always Hit -- fallback: treat undefined as Hit

rankValue :: Rank -> Int
rankValue = \case
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Jack -> 10
    Queen -> 10
    King -> 10
    Ace -> 1
