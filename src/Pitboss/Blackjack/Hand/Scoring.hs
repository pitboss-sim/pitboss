{-# LANGUAGE LambdaCase #-}

module Pitboss.Blackjack.Hand.Scoring where

import Pitboss.Blackjack.Card (Card (..), Rank (..))
import Pitboss.Blackjack.Hand (Hand (..), unHand)
import Pitboss.Blackjack.Hand.Category (HandCategory (..), categorize)

data HandScore
    = Natural
    | Hard Int
    | Soft Int
    | Busted
    deriving (Eq, Show)

scoreValue :: HandScore -> Int
scoreValue = \case
    Natural -> 21
    Hard n -> n
    Soft n -> n
    Busted -> 0

isSoftScore :: HandScore -> Bool
isSoftScore (Soft _) = True
isSoftScore _ = False

isBustedScore :: HandScore -> Bool
isBustedScore Busted = True
isBustedScore _ = False

isNaturalScore :: HandScore -> Bool
isNaturalScore Natural = True
isNaturalScore _ = False

scoreHand :: Hand -> HandScore
scoreHand hand =
    let cards = unHand hand
        category = categorize cards
    in case category of
        Blackjack -> Natural
        _ ->
            let hardTotal = sum (map hardRankValue cards)
                aceCount = length (filter ((== Ace) . rank) cards)
                softCandidate = hardTotal + (aceCount * 10)
            in if softCandidate <= 21 && aceCount > 0
               then if softCandidate == 21 && length cards == 2
                    then Natural
                    else Soft softCandidate
               else if hardTotal <= 21
                    then Hard hardTotal
                    else Busted

hardRankValue :: Card -> Int
hardRankValue (Card r _) = case r of
    Ace -> 1
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

dealerShouldHit :: Bool -> HandScore -> Bool
dealerShouldHit h17Rule score = case score of
    Natural -> False
    Hard n -> n < 17
    Soft n -> n < 17 || (n == 17 && h17Rule)
    Busted -> False

canDouble :: HandScore -> Bool
canDouble = \case
    Hard n -> n >= 9 && n <= 11
    Soft n -> n >= 13 && n <= 18
    _ -> False

canSplit :: Hand -> Bool
canSplit hand =
    let cards = unHand hand
    in case cards of
        [c1, c2] -> rank c1 == rank c2
        _ -> False

compareHands :: HandScore -> HandScore -> Ordering
compareHands dealer player = case (dealer, player) of
    (Busted, Busted) -> EQ
    (Busted, _) -> LT
    (_, Busted) -> GT
    (Natural, Natural) -> EQ
    (Natural, _) -> GT
    (_, Natural) -> LT
    _ -> compare (scoreValue dealer) (scoreValue player)

handTotal :: Hand -> Maybe Int
handTotal hand = case scoreHand hand of
    Busted -> Nothing
    score -> Just (scoreValue score)

isSoft :: Hand -> Bool
isSoft = isSoftScore . scoreHand

toDealerScore :: HandScore -> Maybe (Int, Bool)
toDealerScore = \case
    Natural -> Just (21, False)
    Hard n -> Just (n, False)
    Soft n -> Just (n, True)
    Busted -> Nothing
