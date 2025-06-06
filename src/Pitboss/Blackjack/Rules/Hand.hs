module Pitboss.Blackjack.Rules.Hand where

import Control.Monad (unless, when)
import Pitboss.Blackjack.Rules.Game (canSplitAnotherHand)
import Pitboss.Blackjack.Rules.Hand.Witness
import Pitboss.Blackjack.Types.Core
import Pitboss.Blackjack.Types.GameRuleSet
import Pitboss.Blackjack.Types.Hand
import Pitboss.Blackjack.Types.Hand.Witness

characterize :: [Card] -> SomeHand
characterize cards =
    let witness = computeHandWitness cards
     in SomeHand cards witness

isLegalMove :: Move -> SomeHand -> GameRuleSet -> Bool
isLegalMove move hand rules = case move of
    MHit -> isLegalHit hand
    MStand -> isLegalStand hand
    MDouble -> isLegalDouble hand rules
    MSplit -> isLegalSplit hand rules 0
    MSurrender -> isLegalSurrender rules

isLegalHit :: SomeHand -> Bool
isLegalHit hand =
    let witness = handWitness hand
        score = numericValue witness
     in score < 21 && valueType witness /= BustWitness

isLegalStand :: SomeHand -> Bool
isLegalStand _ = True

isLegalDouble :: SomeHand -> GameRuleSet -> Bool
isLegalDouble hand rules =
    let witness = handWitness hand
        total = numericValue witness
        isInitialHand = length (handCards hand) == 2
        notBlackjack = valueType witness /= BlackjackWitness
     in isInitialHand && notBlackjack && case doubling rules of
            DoubleAny -> True
            Double9_10 -> total == 9 || total == 10
            Double9_11 -> total >= 9 && total <= 11
            Double10_11 -> total == 10 || total == 11

isLegalSplit :: SomeHand -> GameRuleSet -> Int -> Bool
isLegalSplit hand rules splitCount =
    let witness = handWitness hand
        withinSplitLimit = canSplitAnotherHand (splitHands rules) splitCount
        isInitialHand = length (handCards hand) == 2
     in isInitialHand && withinSplitLimit && case structure witness of
            PairWitness Ace ->
                (splitAcesAllowed rules == SplitAces)
                    && (splitCount == 0 || resplitAcesAllowed rules == ResplitAces)
            PairWitness _ -> True
            _ -> False

isLegalSurrender :: GameRuleSet -> Bool
isLegalSurrender rules = case surrender rules of
    NoSurrender -> False
    _ -> True

availableMoves :: SomeHand -> GameRuleSet -> Int -> [Move]
availableMoves hand rules splitCount =
    let moves = []
        addIf condition move acc = if condition then move : acc else acc
     in moves
            & addIf (isLegalHit hand) MHit
            & addIf (isLegalStand hand) MStand
            & addIf (isLegalDouble hand rules) MDouble
            & addIf (isLegalSplit hand rules splitCount) MSplit
            & addIf (isLegalSurrender rules) MSurrender
  where
    (&) = flip ($)

extractSplitCards :: SomeHand -> Maybe (Card, Card)
extractSplitCards hand = case handCards hand of
    [card1, card2] | rank card1 == rank card2 -> Just (card1, card2)
    _ -> Nothing

validateSplitEligibility :: SomeHand -> GameRuleSet -> Int -> Either String ()
validateSplitEligibility hand rules currentSplitCount = do
    let witness = handWitness hand
    unless (length (handCards hand) == 2) $
        Left "Cannot split: not a two-card hand"

    case structure witness of
        PairWitness pairRank -> do
            unless (canSplitAnotherHand (splitHands rules) currentSplitCount) $
                Left "Cannot split: split limit exceeded"

            case pairRank of
                Ace -> do
                    when (splitAcesAllowed rules /= SplitAces) $
                        Left "Cannot split: ace splitting not allowed"
                    when (currentSplitCount > 0 && resplitAcesAllowed rules /= ResplitAces) $
                        Left "Cannot split: ace resplitting not allowed"
                _ -> pure ()
        _ -> Left "Cannot split: hand is not a pair"

splitIntoHands :: SomeHand -> Either String (SomeHand, SomeHand)
splitIntoHands hand = case extractSplitCards hand of
    Nothing -> Left "Cannot split: not a valid pair"
    Just (card1, card2) -> Right (characterize [card1], characterize [card2])

validateMoveInContext :: Move -> SomeHand -> GameRuleSet -> Int -> Either String ()
validateMoveInContext move hand rules splitCount =
    case move of
        MHit ->
            if isLegalHit hand
                then Right ()
                else Left "Cannot hit: hand is 21 or busted"
        MStand -> Right ()
        MDouble ->
            let witness = handWitness hand
             in case completeness witness of
                    FullWitness ->
                        -- exactly 2 cards, never been hit
                        case valueType witness of
                            BlackjackWitness -> Left "Cannot double on blackjack"
                            _ ->
                                if CanDoubleWitness `elem` availableActions witness
                                    then Right ()
                                    else Left "Cannot double: total not allowed by rules"
                    _ -> Left "Cannot double after hitting"
        MSplit -> validateSplitEligibility hand rules splitCount
        MSurrender ->
            if isLegalSurrender rules
                then Right ()
                else Left "Cannot surrender: not allowed by rules"
