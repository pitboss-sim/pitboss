{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Spec.Pitboss.PropertySpec where

import Pitboss.Blackjack
import Pitboss.Causality
import Spec.Pitboss.Helpers ()
import Test.Hspec
import Test.QuickCheck hiding (witness)

-- Generators
genCard :: Gen Card
genCard = Card <$> arbitrary <*> arbitrary

genHand :: Gen [Card]
genHand = do
    n <- choose (2, 5)
    vectorOf n genCard

spec :: Spec
spec = describe "Property Tests" $ do
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

    describe "Dealer rule properties" $ do
        it "dealer never hits on 17 or higher (S17)" $
            property $
                forAll genHand $ \cards ->
                    let hand = characterize cards
                        rules = (gameRuleSet vegas6){soft17 = StandSoft17}
                     in handScore hand >= 17 ==> not (dealerShouldHit rules hand)

        it "dealer must hit below 17" $
            property $
                forAll genHand $ \cards ->
                    let hand = characterize cards
                        score = handScore hand
                        rules = gameRuleSet vegas6
                     in (score < 17 && score > 0) ==> dealerShouldHit rules hand

    describe "FiniteMap properties" $ do
        it "singleton contains exactly one present value" $ property $ \ix val ->
            let fm =
                    singletonFiniteMap ix (Present val) Absent ::
                        FiniteMap PlayerSpotHandIx (Occupancy (EntityId 'PlayerHand))
                presentCount = length [v | (_, Present v) <- toListFiniteMap fm]
             in presentCount == 1

        it "insert overwrites previous value" $ property $ \ix val1 val2 ->
            let fm1 = singletonFiniteMap ix (Present val1) Absent
                fm2 =
                    insertFiniteMap ix (Present val2) fm1 ::
                        FiniteMap PlayerSpotHandIx (Occupancy (EntityId 'PlayerHand))
             in lookupFiniteMap ix fm2 == Just (Present val2)

    describe "Chip arithmetic properties" $ do
        it "chips have additive identity" $ do
            (Chips 0 + Chips 100) `shouldBe` Chips 100
            (Chips 100 + Chips 0) `shouldBe` Chips 100

        it "chip addition is associative" $ property $ \a b c ->
            let chips_a = Chips (abs a)
                chips_b = Chips (abs b)
                chips_c = Chips (abs c)
             in (chips_a + chips_b) + chips_c == chips_a + (chips_b + chips_c)
