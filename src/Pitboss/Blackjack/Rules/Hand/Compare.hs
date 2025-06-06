module Pitboss.Blackjack.Rules.HandComparison where

import Pitboss.Blackjack.Outcomes
import Pitboss.Blackjack.Types

data HandRank = HandRank
    { rankValue :: ValueWitness
    , rankNumeric :: Int
    , rankCardCount :: Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

handRank :: HandWitness -> HandRank
handRank witness =
    HandRank
        { rankValue = valueType witness
        , rankNumeric = case valueType witness of
            BustWitness -> 0
            _ -> numericValue witness
        , rankCardCount = case completeness witness of
            NoneWitness -> 0
            PartialWitness -> 1
            FullWitness -> 2
            ExtendedWitness -> 3
        }

compareHands :: SomeHand -> SomeHand -> Ordering
compareHands hand1 hand2 =
    let rank1 = handRank (handWitness hand1)
        rank2 = handRank (handWitness hand2)
     in compare rank1 rank2

handStrength :: SomeHand -> Int
handStrength hand =
    let witness = handWitness hand
        rank = handRank witness
     in case rankValue rank of
            BustWitness -> 0
            HardWitness -> rankNumeric rank
            SoftWitness -> rankNumeric rank + 100
            BlackjackWitness -> 1000

bestHand :: [SomeHand] -> Maybe SomeHand
bestHand [] = Nothing
bestHand hands = Just $ maximumBy compareHands hands
  where
    maximumBy :: (a -> a -> Ordering) -> [a] -> a
    maximumBy _ [] = error "empty list"
    maximumBy cmp (x : xs) = foldl (\acc y -> if cmp acc y == LT then y else acc) x xs

compareHandsForBout :: SomeHand -> SomeHand -> DetailedOutcome
compareHandsForBout boutPlayerHand dealerHand = case compareHands boutPlayerHand dealerHand of
    GT -> determineWinReason boutPlayerHand dealerHand BoutPlayerWin
    LT -> determineWinReason boutPlayerHand dealerHand BoutDealerWin
    EQ -> pushOutcome

determineWinReason :: SomeHand -> SomeHand -> BoutOutcome -> DetailedOutcome
determineWinReason boutPlayerHand dealerHand winner =
    let boutPlayerWitness = handWitness boutPlayerHand
        dealerWitness = handWitness dealerHand
     in case (valueType boutPlayerWitness, valueType dealerWitness, winner) of
            (_, BustWitness, BoutPlayerWin) -> boutPlayerWinsDealerBust
            (BustWitness, _, BoutDealerWin) -> dealerWinsBoutPlayerBust
            (BlackjackWitness, _, BoutPlayerWin) -> boutPlayerWinsBlackjack
            (_, BlackjackWitness, BoutDealerWin) -> dealerWinsBlackjack
            (_, _, BoutPlayerWin) -> boutPlayerWinsHigher
            (_, _, BoutDealerWin) -> dealerWinsHigher
            _ -> pushOutcome

standardHandComparison :: SomeHand -> SomeHand -> Ordering
standardHandComparison = compareHands

describeHand :: SomeHand -> String
describeHand hand =
    let witness = handWitness hand
        cards = handCards hand
        cardDesc = show (length cards) ++ " cards: " ++ show cards
     in case valueType witness of
            BlackjackWitness -> "Blackjack (" ++ cardDesc ++ ")"
            BustWitness -> "Bust (" ++ cardDesc ++ ")"
            SoftWitness -> "Soft " ++ show (numericValue witness) ++ " (" ++ cardDesc ++ ")"
            HardWitness -> "Hard " ++ show (numericValue witness) ++ " (" ++ cardDesc ++ ")"

handSummary :: SomeHand -> String
handSummary hand =
    let witness = handWitness hand
        value = numericValue witness
     in case valueType witness of
            BlackjackWitness -> "BJ"
            BustWitness -> "BUST"
            SoftWitness -> "S" ++ show value
            HardWitness -> show value

compareByOutcome :: DetailedOutcome -> DetailedOutcome -> Ordering
compareByOutcome out1 out2 = case (outcome out1, outcome out2) of
    (BoutPlayerWin, BoutPlayerWin) -> EQ
    (BoutDealerWin, BoutDealerWin) -> EQ
    (Push, Push) -> EQ
    (BoutPlayerWin, _) -> GT
    (Push, BoutDealerWin) -> GT
    (BoutDealerWin, _) -> LT

sortHandsByStrength :: [SomeHand] -> [SomeHand]
sortHandsByStrength hands =
    let handPairs = [(hand, handStrength hand) | hand <- hands]
        sortedPairs = sortBy (\(_, s1) (_, s2) -> compare s2 s1) handPairs
     in map fst sortedPairs
  where
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy _ [] = []
    sortBy cmp (x : xs) =
        let smaller = [y | y <- xs, cmp y x == LT]
            larger = [y | y <- xs, cmp y x /= LT]
         in sortBy cmp smaller ++ [x] ++ sortBy cmp larger
