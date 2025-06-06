{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Rules.Validate where

import Data.Aeson.Types
import Data.Either (lefts)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Rules.Hand
import Pitboss.Blackjack.Rules.Hand.Witness
import Pitboss.Blackjack.Types

data ValidationError
    = IllegalMove Move String
    | DomainConstraintViolation String
    | WitnessInconsistency String
    | FSMTransitionError String
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

validateMoveWithContext :: Move -> SomeHand -> GameRuleSet -> Int -> Either ValidationError ()
validateMoveWithContext move hand rules splitCount = do
    validateWitnessConsistency hand
    validateDomainConstraints move hand rules splitCount

validateDomainConstraints :: Move -> SomeHand -> GameRuleSet -> Int -> Either ValidationError ()
validateDomainConstraints move hand rules splitCount =
    case validateMoveInContext move hand rules splitCount of
        Right () -> Right ()
        Left err -> Left $ IllegalMove move err

validateWitnessConsistency :: SomeHand -> Either ValidationError ()
validateWitnessConsistency hand =
    let cards = handCards hand
        witness = handWitness hand
        recomputedWitness = computeHandWitness cards
     in if witness == recomputedWitness
            then Right ()
            else
                Left $
                    WitnessInconsistency $
                        "Hand witness inconsistent: expected "
                            ++ show recomputedWitness
                            ++ ", got "
                            ++ show witness

validateHandProgression :: [SomeHand] -> Either ValidationError ()
validateHandProgression [] = Right ()
validateHandProgression [_] = Right ()
validateHandProgression (h1 : h2 : rest) = do
    validateHandTransition h1 h2
    validateHandProgression (h2 : rest)

validateHandTransition :: SomeHand -> SomeHand -> Either ValidationError ()
validateHandTransition oldHand newHand =
    let oldCards = handCards oldHand
        newCards = handCards newHand
        oldCount = length oldCards
        newCount = length newCards
     in case compare newCount oldCount of
            LT -> Left $ DomainConstraintViolation "Hand cannot lose cards"
            EQ ->
                if oldCards == newCards
                    then Right ()
                    else Left $ DomainConstraintViolation "Hand cards changed without addition"
            GT ->
                if take oldCount newCards == oldCards
                    then validateNewCard (drop oldCount newCards)
                    else Left $ DomainConstraintViolation "Hand cards reordered during transition"

validateNewCard :: [Card] -> Either ValidationError ()
validateNewCard [_] = Right ()
validateNewCard [] = Left $ DomainConstraintViolation "Expected new card but none found"
validateNewCard _ = Left $ DomainConstraintViolation "Multiple cards added in single transition"

validateMoveSequence :: [Move] -> [SomeHand] -> GameRuleSet -> Int -> Either ValidationError ()
validateMoveSequence moves hands rules splitCount = do
    validateHandProgression hands
    let moveHandPairs = zip moves hands
    mapM_ (\(move, hand) -> validateDomainConstraints move hand rules splitCount) moveHandPairs

validateHandIntegrity :: SomeHand -> Either ValidationError ()
validateHandIntegrity hand = do
    validateWitnessConsistency hand
    validateHandStructure hand

validateHandStructure :: SomeHand -> Either ValidationError ()
validateHandStructure hand =
    let cards = handCards hand
        witness = handWitness hand
     in case (cards, structure witness) of
            ([], EmptyWitness) -> Right ()
            ([_], SingletonWitness) -> Right ()
            ([c1, c2], PairWitness r)
                | rank c1 == r && rank c2 == r -> Right ()
                | otherwise -> Left $ WitnessInconsistency "Pair witness doesn't match card ranks"
            ([c1, c2], NonPairWitness)
                | rank c1 /= rank c2 -> Right ()
                | otherwise -> Left $ WitnessInconsistency "NonPair witness but cards form a pair"
            (cs, NonPairWitness)
                | length cs > 2 -> Right ()
                | otherwise -> Left $ WitnessInconsistency "NonPair witness with insufficient cards"
            _ -> Left $ WitnessInconsistency "Hand structure doesn't match witness"

isValidMove :: Move -> SomeHand -> GameRuleSet -> Int -> Bool
isValidMove move hand rules splitCount =
    case validateMoveWithContext move hand rules splitCount of
        Right () -> True
        Left _ -> False

getAllValidMoves :: SomeHand -> GameRuleSet -> Int -> [Move]
getAllValidMoves hand rules splitCount =
    let allMoves = [MHit, MStand, MDouble, MSplit, MSurrender]
     in filter (\move -> isValidMove move hand rules splitCount) allMoves

validateMultipleMoves :: [(Move, SomeHand, GameRuleSet, Int)] -> [ValidationError]
validateMultipleMoves = lefts . map (\(m, h, r, s) -> validateMoveWithContext m h r s)
