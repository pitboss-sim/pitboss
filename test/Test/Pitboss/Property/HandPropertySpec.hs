{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Property.HandPropertySpec where

import Pitboss.Blackjack
import Test.Hspec
import Test.Pitboss.TestUtils ()
import Test.QuickCheck hiding (witness)

-- Generators
genCard :: Gen Card
genCard = Card <$> arbitrary <*> arbitrary

genHand :: Gen [Card]
genHand = do
    n <- choose (2, 5)
    vectorOf n genCard

spec :: Spec
spec = describe "Hand Property Tests" $ do
    describe "Hand scoring properties" $ do
        it "score is always between 0 and 21 or bust" $
            property $
                forAll genHand $ \cards ->
                    let hand = characterize cards
                        score = handScore hand
                     in score >= 0 && score <= 21
                            || case hand of
                                SomeHand h -> case witness h of
                                    BustWitness -> True
                                    _ -> False

        it "adding a card never decreases hard count" $
            property $
                forAll genHand $ \cards ->
                    forAll genCard $ \newCard ->
                        let hardValue1 = sum $ map (rankValue . rank) cards
                            hardValue2 = sum $ map (rankValue . rank) (cards ++ [newCard])
                         in hardValue2 >= hardValue1

        it "blackjack requires exactly 2 cards" $
            property $
                forAll genHand $ \cards ->
                    case characterize cards of
                        SomeHand h -> case witness h of
                            BlackjackWitness -> length cards == 2
                            _ -> True
