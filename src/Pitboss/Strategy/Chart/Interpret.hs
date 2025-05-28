{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Strategy.Chart.Interpret where

import Data.List (find)
import Data.Map.Strict qualified as Map
import Pitboss.Blackjack.Card (Rank (..))
import Pitboss.Blackjack.Hand
import Pitboss.Strategy.Chart.Types
import Pitboss.Strategy.Types
import Pitboss.Blackjack.Offering
import Pitboss.Blackjack.Play (canSplitSomeHand, canDoubleSomeHand)

lookupDecisionTyped :: StrategyChart -> SomeHand -> Rank -> Offering -> Maybe Decision
lookupDecisionTyped chart hand upcard _offering = do
    entry <- findMatchingEntry chart hand
    moveCode <- Map.lookup upcard (moves entry)
    pure (toDecision moveCode)

findMatchingEntry :: StrategyChart -> SomeHand -> Maybe ChartEntry
findMatchingEntry chart hand@(SomeHand h) =
    case witness h of
        BlackjackWitness ->
            find (\e -> handKind e == BlackjackHand) chart
        PairWitness -> case extractPairRank hand of
            Just Ace -> find (\e -> handKind e == PairHand && kindValue e == Just 1) chart
            Just r -> find (\e -> handKind e == PairHand && kindValue e == Just (rankValue r)) chart
            Nothing -> Nothing
        SoftWitness ->
            let score = handScore hand
            in find (\e -> handKind e == SoftHand && kindValue e == Just score) chart
        HardWitness ->
            let score = handScore hand
            in find (\e -> handKind e == HardHand && kindValue e == Just score) chart
        TwentyOneWitness ->
            find (\e -> handKind e == HardHand && kindValue e == Just 21) chart
        BustWitness ->
            find (\e -> handKind e == HardHand && kindValue e == Just 0) chart

toDecision :: MoveCode -> Decision
toDecision MoveHit = Always Hit
toDecision MoveStand = Always Stand
toDecision MoveDoubleOrHit = Prefer Double (Else Hit)
toDecision MoveDoubleOrStand = Prefer Double (Else Stand)
toDecision MoveSplit = Always Split
toDecision MoveSplitOrHit = Prefer Split (Else Hit)
toDecision MoveSurrenderOrStand = Prefer Surrender (Else Stand)
toDecision MoveUndefined = Always Hit

rankValue :: Rank -> Int
rankValue Ace = 1
rankValue Two = 2
rankValue Three = 3
rankValue Four = 4
rankValue Five = 5
rankValue Six = 6
rankValue Seven = 7
rankValue Eight = 8
rankValue Nine = 9
rankValue Ten = 10
rankValue Jack = 10
rankValue Queen = 10
rankValue King = 10

validateDecision :: SomeHand -> Decision -> Offering -> Bool
validateDecision hand decision offering = case decision of
    Always Split -> canSplitSomeHand hand 0 offering
    Prefer Split _ -> canSplitSomeHand hand 0 offering
    Always Double -> canDoubleSomeHand hand offering
    Prefer Double _ -> canDoubleSomeHand hand offering
    _ -> True

safeDecisionLookup :: StrategyChart -> SomeHand -> Rank -> Offering -> Decision
safeDecisionLookup chart hand upcard offering =
    case lookupDecisionTyped chart hand upcard offering of
        Just decision | validateDecision hand decision offering -> decision
        Just decision -> fallbackForInvalidDecision decision
        Nothing -> Always Hit

fallbackForInvalidDecision :: Decision -> Decision
fallbackForInvalidDecision (Prefer _ (Else fallback)) = Always fallback
fallbackForInvalidDecision _ = Always Hit
