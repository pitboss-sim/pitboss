{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Property.HandPropertySpec where

import Pitboss.Blackjack
import Test.Hspec
import Test.Pitboss.TestUtils ()
import Test.QuickCheck hiding (witness)

spec :: Spec
spec = describe "Hand Property Tests" $ do
    describe "hand scoring properties" $ do
        it "score is 0-21 unless bust" $
            property $
                forAll genValidHand $ \cards ->
                    let hand = characterize cards
                        score = handScore hand
                     in case valueType (handWitness hand) of
                            BustWitness -> score > 21
                            _ -> score >= 0 && score <= 21

        it "hard count never decreases when adding cards" $
            property $
                forAll genValidHand $ \cards ->
                    forAll arbitrary $ \newCard ->
                        let hardValue1 = sum $ map cardValue cards
                            hardValue2 = sum $ map cardValue (cards ++ [newCard])
                         in hardValue2 >= hardValue1

        it "ace handling respects value bounds" $
            property $
                forAll genHandWithAces $ \cards ->
                    let hand = characterize cards
                        score = handScore hand
                        minValue = sum $ map minCardValue cards
                     in case valueType (handWitness hand) of
                            BustWitness -> score > 21
                            _ -> score >= minValue && score <= 21

    describe "hand classification properties" $ do
        it "blackjack requires exactly ace and ten-value" $
            property $
                forAll genValidHand $ \cards ->
                    case valueType (handWitness (characterize cards)) of
                        BlackjackWitness ->
                            length cards == 2
                                && hasAce cards
                                && hasTenValue cards
                        _ -> True

        it "pairs have exactly two cards of same rank" $
            property $
                forAll genValidHand $ \cards ->
                    case structure (handWitness (characterize cards)) of
                        PairWitness _ ->
                            length cards == 2
                                && case cards of
                                    [Card r1 _, Card r2 _] -> r1 == r2
                                    _ -> False
                        _ -> True

        it "bust hands always score over 21" $
            property $
                forAll genBustHand $ \cards ->
                    handScore (characterize cards) > 21

        it "soft hands have flexible ace counted as 11" $
            property $
                forAll genSoftHand $ \cards ->
                    let hand = characterize cards
                        witness = handWitness hand
                     in case valueType witness of
                            SoftWitness ->
                                hasAce cards
                                    && handScore hand < 21 -- Must be less than 21 to be soft
                                    && handScore hand > sum (map minCardValue cards)
                            _ -> True

genValidHand :: Gen [Card]
genValidHand = do
    n <- choose (1, 6)
    vectorOf n arbitrary

genHandWithAces :: Gen [Card]
genHandWithAces = do
    numAces <- choose (1, 2)
    numOthers <- choose (1, 3)
    aces <- vectorOf numAces (Card Ace <$> arbitrary)
    others <- vectorOf numOthers (Card <$> elements nonAceRanks <*> arbitrary)
    pure (aces ++ others)
  where
    nonAceRanks = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]

genBustHand :: Gen [Card]
genBustHand = do
    tenCards <- choose (3, 4)
    vectorOf tenCards (Card <$> elements [Ten, Jack, Queen, King] <*> arbitrary)

-- Fixed generator to only create actual soft hands
genSoftHand :: Gen [Card]
genSoftHand = do
    ace <- Card Ace <$> arbitrary
    -- Generate low cards that when combined with ace (as 11) stay under 21
    lowCards <- do
        n <- choose (1, 3)
        cards <- vectorOf n (Card <$> elements [Two, Three, Four, Five, Six, Seven, Eight, Nine] <*> arbitrary)
        let totalLow = sum $ map (rankValue . rank) cards
        -- Ensure ace can be counted as 11 AND total stays under 21
        if totalLow + 11 < 21 -- Strictly less than 21 to ensure soft
            then pure cards
            else pure [Card Two Hearts] -- Fallback: Ace + 2 = soft 13
    pure (ace : lowCards)

hasAce :: [Card] -> Bool
hasAce = any (\(Card r _) -> r == Ace)

hasTenValue :: [Card] -> Bool
hasTenValue = any (\(Card r _) -> rankValue r == 10)

minCardValue :: Card -> Int
minCardValue (Card Ace _) = 1
minCardValue card = cardValue card
