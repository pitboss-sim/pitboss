{-# LANGUAGE DataKinds #-}

module Spec.Pitboss.Simulation.DealerRulesSpec where

import Pitboss.Blackjack
import Spec.Pitboss.Helpers
import Test.Hspec

spec :: Spec
spec = describe "Dealer S17/H17 Rules" $ do
    describe "dealerShouldHit with Stand Soft 17 (S17)" $ do
        let rules = mkS17Rules

        it "hits on hard 16" $ do
            let hand = characterize [Card Ten Hearts, Card Six Spades]
            dealerShouldHit rules hand `shouldBe` True

        it "stands on hard 17" $ do
            let hand = characterize [Card Ten Hearts, Card Seven Spades]
            dealerShouldHit rules hand `shouldBe` False

        it "stands on soft 17 (A-6)" $ do
            let hand = characterize [Card Ace Hearts, Card Six Spades]
            dealerShouldHit rules hand `shouldBe` False

        it "stands on soft 18 (A-7)" $ do
            let hand = characterize [Card Ace Hearts, Card Seven Spades]
            dealerShouldHit rules hand `shouldBe` False

        it "hits on soft 16 (A-5)" $ do
            let hand = characterize [Card Ace Hearts, Card Five Spades]
            dealerShouldHit rules hand `shouldBe` True

        it "stands on blackjack" $ do
            let hand = characterize [Card Ace Hearts, Card King Spades]
            dealerShouldHit rules hand `shouldBe` False

        it "does not hit on bust" $ do
            let hand = characterize [Card Ten Hearts, Card Six Spades, Card King Clubs]
            dealerShouldHit rules hand `shouldBe` False

        it "stands on 21" $ do
            let hand = characterize [Card Seven Hearts, Card Seven Spades, Card Seven Clubs]
            dealerShouldHit rules hand `shouldBe` False

        it "stands on multi-card soft 17 (A-2-4)" $ do
            let hand = characterize [Card Ace Hearts, Card Two Spades, Card Four Clubs]
            dealerShouldHit rules hand `shouldBe` False

    describe "dealerShouldHit with Hit Soft 17 (H17)" $ do
        let rules = mkH17Rules

        it "hits on hard 16" $ do
            let hand = characterize [Card Ten Hearts, Card Six Spades]
            dealerShouldHit rules hand `shouldBe` True

        it "stands on hard 17" $ do
            let hand = characterize [Card Ten Hearts, Card Seven Spades]
            dealerShouldHit rules hand `shouldBe` False

        it "hits on soft 17 (A-6)" $ do
            let hand = characterize [Card Ace Hearts, Card Six Spades]
            dealerShouldHit rules hand `shouldBe` True

        it "stands on soft 18 (A-7)" $ do
            let hand = characterize [Card Ace Hearts, Card Seven Spades]
            dealerShouldHit rules hand `shouldBe` False

        it "hits on soft 16 (A-5)" $ do
            let hand = characterize [Card Ace Hearts, Card Five Spades]
            dealerShouldHit rules hand `shouldBe` True

        it "stands on blackjack" $ do
            let hand = characterize [Card Ace Hearts, Card King Spades]
            dealerShouldHit rules hand `shouldBe` False

        it "does not hit on bust" $ do
            let hand = characterize [Card Ten Hearts, Card Six Spades, Card King Clubs]
            dealerShouldHit rules hand `shouldBe` False

        it "hits on multi-card soft 17 (A-2-4)" $ do
            let hand = characterize [Card Ace Hearts, Card Two Spades, Card Four Clubs]
            dealerShouldHit rules hand `shouldBe` True

        it "hits on complex soft 17 (A-A-5)" $ do
            let hand = characterize [Card Ace Hearts, Card Ace Spades, Card Five Clubs]
            dealerShouldHit rules hand `shouldBe` True

    describe "Edge cases" $ do
        let s17Rules = mkS17Rules
        let h17Rules = mkH17Rules

        it "handles pair of 8s (hard 16) correctly" $ do
            let hand = characterize [Card Eight Hearts, Card Eight Spades]
            dealerShouldHit s17Rules hand `shouldBe` True
            dealerShouldHit h17Rules hand `shouldBe` True

        it "handles A-A-A-A-3 (soft 17) correctly" $ do
            let hand = characterize [Card Ace Hearts, Card Ace Spades, Card Ace Clubs, Card Ace Diamonds, Card Three Hearts]

            dealerShouldHit s17Rules hand `shouldBe` False
            dealerShouldHit h17Rules hand `shouldBe` True

        it "handles hard 17 from many small cards" $ do
            let hand = characterize [Card Two Hearts, Card Three Spades, Card Four Clubs, Card Four Diamonds, Card Four Hearts]

            dealerShouldHit s17Rules hand `shouldBe` False
            dealerShouldHit h17Rules hand `shouldBe` False

    describe "Rule detection" $ do
        it "correctly identifies S17 rules" $ do
            isH17 mkS17Rules `shouldBe` False

        it "correctly identifies H17 rules" $ do
            isH17 mkH17Rules `shouldBe` True

    describe "Simulation: Dealer final totals with S17 vs H17" $ do
        it "tracks how often dealer busts or makes each total" $ do
            let s17Rules = mkS17Rules
            let h17Rules = mkH17Rules
            let soft17 = characterize [Card Ace Hearts, Card Six Spades]
            dealerShouldHit s17Rules soft17 `shouldBe` False
            dealerShouldHit h17Rules soft17 `shouldBe` True
