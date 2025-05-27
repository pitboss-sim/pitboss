{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Strategy.Chart.Interpret where

import Data.Map.Strict qualified as Map
import Pitboss.Blackjack.Card (Rank (..))
import Pitboss.Blackjack.Hand
import Pitboss.Blackjack.Hand.Operations (cannotDouble, cannotSplit, extractPairRank)
import Pitboss.Strategy.Chart.Types
import Pitboss.Strategy.Types

lookupDecisionTyped :: StrategyChart -> SomeHand -> Rank -> Maybe Decision
lookupDecisionTyped chart hand upcard =
    let chartMap = Map.fromList [(handPrefix e, e) | e <- chart]
        prefix = handToPrefix hand
     in do
            entry <- Map.lookup prefix chartMap
            moveCode <- Map.lookup upcard (moves entry)
            pure (toDecision moveCode)

handToPrefix :: SomeHand -> HandPrefix
handToPrefix (SomeHand hand) = case witness hand of
    BlackjackWitness -> PT
    TwentyOneWitness -> H 21
    BustWitness -> H (handScore (SomeHand hand))
    PairWitness -> case extractPairRank (SomeHand hand) of
        Just Ace -> PA
        Just r -> P (rankValue r)
        Nothing -> error "handToPrefix: pair hand without pair rank"
    SoftWitness -> A (handScore (SomeHand hand))
    HardWitness -> H (handScore (SomeHand hand))

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
rankValue Ace = 1

validateDecision :: SomeHand -> Decision -> Bool
validateDecision hand decision = case decision of
    Always Split -> not (cannotSplit hand)
    Prefer Split _ -> not (cannotSplit hand)
    Always Double -> not (cannotDouble hand)
    Prefer Double _ -> not (cannotDouble hand)
    _ -> True

safeDecisionLookup :: StrategyChart -> SomeHand -> Rank -> Decision
safeDecisionLookup chart hand upcard =
    case lookupDecisionTyped chart hand upcard of
        Just decision | validateDecision hand decision -> decision
        Just decision -> fallbackForInvalidDecision decision
        Nothing -> Always Hit

fallbackForInvalidDecision :: Decision -> Decision
fallbackForInvalidDecision (Prefer _ (Else fallback)) = Always fallback
fallbackForInvalidDecision _ = Always Hit
