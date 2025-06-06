{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Strategy.Chart.Interpret where

import Data.Aeson.Types
import Data.List (find)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Pitboss.Blackjack.Rules.Hand
import Pitboss.Blackjack.Strategy.Chart.Types
import Pitboss.Blackjack.Strategy.Types
import Pitboss.Blackjack.Types

data WitnessKey = WitnessKey
    { keyValueType :: ValueWitness
    , keyStructure :: StructureWitness
    , keyNumericValue :: Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Ord WitnessKey where
    compare (WitnessKey v1 s1 n1) (WitnessKey v2 s2 n2) =
        case compare v1 v2 of
            EQ -> case compare s1 s2 of
                EQ -> compare n1 n2
                other -> other
            other -> other

witnessToKey :: HandWitness -> WitnessKey
witnessToKey witness =
    WitnessKey
        { keyValueType = valueType witness
        , keyStructure = structure witness
        , keyNumericValue = numericValue witness
        }

lookupDecision :: StrategyChart -> SomeHand -> Rank -> Offering -> Maybe Decision
lookupDecision chart hand upcard offering = do
    entry <- findMatchingEntryByWitness chart hand
    moveCode <- Map.lookup upcard (moves entry)
    let decision = toDecision moveCode
    pure $
        if validateDecisionUnified hand decision (gameRuleSet offering)
            then decision
            else fallbackForInvalidDecision decision

findMatchingEntryByWitness :: StrategyChart -> SomeHand -> Maybe ChartEntry
findMatchingEntryByWitness chart hand =
    let witness = handWitness hand
        key = witnessToKey witness
     in find (matchesWitnessKey key) chart

matchesWitnessKey :: WitnessKey -> ChartEntry -> Bool
matchesWitnessKey key entry = case keyValueType key of
    BlackjackWitness ->
        handKind entry == BlackjackHand
    _ -> case keyStructure key of
        PairWitness rank ->
            let pairValue = if rank == Ace then 1 else rankValue rank
             in handKind entry == PairHand && kindValue entry == Just pairValue
        _ -> case keyValueType key of
            SoftWitness ->
                handKind entry == SoftHand && kindValue entry == Just (keyNumericValue key)
            _ ->
                handKind entry == HardHand && kindValue entry == Just (keyNumericValue key)

toDecision :: MoveCode -> Decision
toDecision MoveHit = Always MHit
toDecision MoveStand = Always MStand
toDecision MoveDoubleOrHit = Prefer MDouble (Else MHit)
toDecision MoveDoubleOrStand = Prefer MDouble (Else MStand)
toDecision MoveSplit = Always MSplit
toDecision MoveSplitOrHit = Prefer MSplit (Else MHit)
toDecision MoveSurrenderOrStand = Prefer MSurrender (Else MStand)
toDecision MoveUndefined = Always MHit

validateDecisionUnified :: SomeHand -> Decision -> GameRuleSet -> Bool
validateDecisionUnified hand decision gameRuleSet = case decision of
    Always MSplit -> isLegalSplit hand gameRuleSet 0
    Prefer MSplit _ -> isLegalSplit hand gameRuleSet 0
    Always MDouble -> isLegalDouble hand gameRuleSet
    Prefer MDouble _ -> isLegalDouble hand gameRuleSet
    Always move -> isLegalMove move hand gameRuleSet
    Prefer move _ -> isLegalMove move hand gameRuleSet

safeDecisionLookup :: StrategyChart -> SomeHand -> Rank -> Offering -> Decision
safeDecisionLookup chart hand upcard offering =
    case lookupDecision chart hand upcard offering of
        Just decision -> decision
        Nothing -> Always MHit

fallbackForInvalidDecision :: Decision -> Decision
fallbackForInvalidDecision (Prefer _ (Else fallback)) = Always fallback
fallbackForInvalidDecision _ = Always MHit
