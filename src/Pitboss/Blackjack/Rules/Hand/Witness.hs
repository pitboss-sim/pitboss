module Pitboss.Blackjack.Rules.Hand.Witness where

import Pitboss.Blackjack.Rules.Game (canSplitAnotherHand)
import Pitboss.Blackjack.Types.Core
import Pitboss.Blackjack.Types.GameRuleSet
import Pitboss.Blackjack.Types.Hand.Witness

computeHandWitness :: [Card] -> HandWitness
computeHandWitness cards =
    let ranks = map rank cards
        aceCount = length (filter (== Ace) ranks)
        nonAceSum = sum (map rankValue (filter (/= Ace) ranks))

        (finalValue, usingSoftAce) = bestAceValue nonAceSum aceCount

        valueWit = computeValueWitness finalValue usingSoftAce (length cards)
        structureWit = computeStructureWitness ranks
        completenessWit = computeCompletenessWitness (length cards)
        actionsWit = computeAvailableActions finalValue usingSoftAce ranks
     in HandWitness
            { valueType = valueWit
            , structure = structureWit
            , completeness = completenessWit
            , availableActions = actionsWit
            , numericValue = finalValue
            }

computeValueWitness :: Int -> Bool -> Int -> ValueWitness
computeValueWitness finalValue usingSoftAce cardCount
    | finalValue > 21 = BustWitness
    | finalValue == 21 && cardCount == 2 = BlackjackWitness
    | finalValue == 21 = HardWitness
    | usingSoftAce && finalValue < 21 = SoftWitness
    | otherwise = HardWitness

computeStructureWitness :: [Rank] -> StructureWitness
computeStructureWitness [] = EmptyWitness
computeStructureWitness [_] = SingletonWitness
computeStructureWitness [r1, r2] | r1 == r2 = PairWitness r1
computeStructureWitness [_, _] = NonPairWitness
computeStructureWitness _ = NonPairWitness

computeCompletenessWitness :: Int -> CompletenessWitness
computeCompletenessWitness 0 = NoneWitness
computeCompletenessWitness 1 = PartialWitness
computeCompletenessWitness 2 = FullWitness
computeCompletenessWitness _ = ExtendedWitness

computeAvailableActions :: Int -> Bool -> [Rank] -> [ActionWitness]
computeAvailableActions finalValue _ _ | finalValue > 21 = []
computeAvailableActions finalValue _ _ | finalValue == 21 = [CanStandWitness]
computeAvailableActions _ _ ranks =
    [CanHitWitness, CanStandWitness]
        ++ [CanDoubleWitness | length ranks == 2]
        ++ [CanSplitWitness | isPair ranks]
        ++ [CanSurrenderWitness | length ranks == 2]
  where
    isPair [r1, r2] = r1 == r2
    isPair _ = False

computeContextualWitness :: [Card] -> GameRuleSet -> Int -> HandWitness
computeContextualWitness cards rules splitCount =
    let baseWitness = computeHandWitness cards
        contextualActions = filterContextualActions baseWitness rules splitCount
     in baseWitness{availableActions = contextualActions}

filterContextualActions :: HandWitness -> GameRuleSet -> Int -> [ActionWitness]
filterContextualActions witness rules splitCount =
    filter (isActionValid witness rules splitCount) (availableActions witness)

isActionValid :: HandWitness -> GameRuleSet -> Int -> ActionWitness -> Bool
isActionValid witness rules splitCount action = case action of
    CanHitWitness -> numericValue witness < 21
    CanStandWitness -> True
    CanDoubleWitness -> case doubling rules of
        DoubleAny -> True
        Double9_10 -> numericValue witness `elem` [9, 10]
        Double9_11 -> numericValue witness `elem` [9, 10, 11]
        Double10_11 -> numericValue witness `elem` [10, 11]
    CanSplitWitness -> case structure witness of
        PairWitness Ace ->
            splitAcesAllowed rules == SplitAces
                && (splitCount == 0 || resplitAcesAllowed rules == ResplitAces)
                && canSplitAnotherHand (splitHands rules) splitCount
        PairWitness _ -> canSplitAnotherHand (splitHands rules) splitCount
        _ -> False
    CanSurrenderWitness -> surrender rules /= NoSurrender

bestAceValue :: Int -> Int -> (Int, Bool)
bestAceValue nonAceSum aceCount
    | aceCount == 0 = (nonAceSum, False)
    | nonAceSum + 11 + (aceCount - 1) <= 21 = (nonAceSum + 11 + (aceCount - 1), True)
    | otherwise = (nonAceSum + aceCount, False)
