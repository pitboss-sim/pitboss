{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Unit.Domain.HandSpec where

import Pitboss.Blackjack
import Test.Hspec

spec :: Spec
spec = describe "Hand Characterization and Scoring" $ do
    describe "characterize" $ do
        it "identifies blackjack correctly" $ do
            let hand = characterize [Card Ace Hearts, Card King Spades]
            case hand of
                SomeHand h -> case witness h of
                    BlackjackWitness -> pure ()
                    other -> expectationFailure $ "Expected BlackjackWitness but got: " ++ show other

        it "identifies soft hands correctly" $ do
            let hand = characterize [Card Ace Hearts, Card Six Spades]
            case hand of
                SomeHand h -> case witness h of
                    SoftWitness -> pure ()
                    other -> expectationFailure $ "Expected SoftWitness but got: " ++ show other

        it "identifies hard hands correctly" $ do
            let hand = characterize [Card Ten Hearts, Card Seven Spades]
            case hand of
                SomeHand h -> case witness h of
                    HardWitness -> pure ()
                    other -> expectationFailure $ "Expected HardWitness but got: " ++ show other

        it "identifies pairs correctly" $ do
            let hand = characterize [Card Eight Hearts, Card Eight Spades]
            case hand of
                SomeHand h -> case witness h of
                    PairWitness -> pure ()
                    other -> expectationFailure $ "Expected PairWitness but got: " ++ show other

        it "identifies bust hands correctly" $ do
            let hand = characterize [Card Ten Hearts, Card Six Spades, Card King Clubs]
            case hand of
                SomeHand h -> case witness h of
                    BustWitness -> pure ()
                    other -> expectationFailure $ "Expected BustWitness but got: " ++ show other

    describe "handScore" $ do
        it "scores blackjack as 21" $ do
            handScore (characterize [Card Ace Hearts, Card King Spades]) `shouldBe` 21

        it "scores soft 17 as 17" $ do
            handScore (characterize [Card Ace Hearts, Card Six Spades]) `shouldBe` 17

        it "scores hard 17 as 17" $ do
            handScore (characterize [Card Ten Hearts, Card Seven Spades]) `shouldBe` 17

        it "scores bust hands as 0" $ do
            handScore (characterize [Card Ten Hearts, Card Six Spades, Card King Clubs]) `shouldBe` 0

        it "handles multiple aces correctly" $ do
            handScore (characterize [Card Ace Hearts, Card Ace Spades, Card Nine Clubs]) `shouldBe` 21
            handScore (characterize [Card Ace Hearts, Card Ace Spades, Card Ace Clubs, Card Eight Diamonds]) `shouldBe` 21

    describe "edge cases" $ do
        it "empty hand is hard 0" $ do
            let hand = characterize []
            handScore hand `shouldBe` 0
            case hand of
                SomeHand h -> case witness h of
                    HardWitness -> pure ()
                    other -> expectationFailure $ "Expected HardWitness but got: " ++ show other

        it "five card 21 is not blackjack" $ do
            let hand = characterize [Card Two Hearts, Card Three Spades, Card Four Clubs, Card Five Diamonds, Card Seven Hearts]
            handScore hand `shouldBe` 21
            case hand of
                SomeHand h -> case witness h of
                    TwentyOneWitness -> pure ()
                    other -> expectationFailure $ "Expected TwentyOneWitness but got: " ++ show other

    describe "extractPairRank" $ do
        it "extracts rank from a pair" $ do
            extractPairRank (characterize [Card Eight Hearts, Card Eight Spades]) `shouldBe` Just Eight

        it "returns Nothing for non-pairs" $ do
            extractPairRank (characterize [Card Eight Hearts, Card Nine Spades]) `shouldBe` Nothing

        it "works for face card pairs" $ do
            extractPairRank (characterize [Card King Hearts, Card King Spades]) `shouldBe` Just King

        it "works for ace pairs" $ do
            extractPairRank (characterize [Card Ace Hearts, Card Ace Spades]) `shouldBe` Just Ace
