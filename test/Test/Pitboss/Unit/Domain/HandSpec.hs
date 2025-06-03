{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Unit.Domain.HandSpec where

import Pitboss.Blackjack
import Test.Hspec

spec :: Spec
spec = describe "Hand Characterization and Scoring" $ do
    describe "characterize" $ do
        it "identifies blackjack correctly" $ do
            characterize [Card Ace Hearts, Card King Spades] `shouldHaveWitness` BlackjackWitness

        it "identifies soft hands correctly" $ do
            characterize [Card Ace Hearts, Card Six Spades] `shouldHaveWitness` SoftWitness

        it "identifies hard hands correctly" $ do
            characterize [Card Ten Hearts, Card Seven Spades] `shouldHaveWitness` HardWitness

        it "identifies pairs correctly" $ do
            characterize [Card Eight Hearts, Card Eight Spades] `shouldHaveStructure` PairWitness Eight

        it "identifies bust hands correctly" $ do
            characterize [Card Ten Hearts, Card Six Spades, Card King Clubs] `shouldHaveWitness` BustWitness

    describe "handScore" $ do
        it "scores blackjack as 21" $ do
            handScore (characterize [Card Ace Hearts, Card King Spades]) `shouldBe` 21

        it "scores soft 17 as 17" $ do
            handScore (characterize [Card Ace Hearts, Card Six Spades]) `shouldBe` 17

        it "scores hard 17 as 17" $ do
            handScore (characterize [Card Ten Hearts, Card Seven Spades]) `shouldBe` 17

        it "scores bust hands as actual total" $ do
            handScore (characterize [Card Ten Hearts, Card Six Spades, Card King Clubs]) `shouldBe` 26

        it "handles multiple aces correctly" $ do
            handScore (characterize [Card Ace Hearts, Card Ace Spades, Card Nine Clubs]) `shouldBe` 21
            handScore (characterize [Card Ace Hearts, Card Ace Spades, Card Ace Clubs, Card Eight Diamonds]) `shouldBe` 21

        it "handles soft ace transitions" $ do
            handScore (characterize [Card Ace Hearts, Card Seven Spades, Card Five Clubs]) `shouldBe` 13

    describe "extractPairRank" $ do
        it "extracts rank from a pair" $ do
            extractPairRank (characterize [Card Eight Hearts, Card Eight Spades]) `shouldBe` Just Eight

        it "returns Nothing for non-pairs" $ do
            extractPairRank (characterize [Card Eight Hearts, Card Nine Spades]) `shouldBe` Nothing

        it "works for face card pairs" $ do
            extractPairRank (characterize [Card King Hearts, Card King Spades]) `shouldBe` Just King

        it "works for ace pairs" $ do
            extractPairRank (characterize [Card Ace Hearts, Card Ace Spades]) `shouldBe` Just Ace

    describe "edge cases" $ do
        it "empty hand is hard 0" $ do
            let hand = characterize []
            handScore hand `shouldBe` 0
            hand `shouldHaveWitness` HardWitness

        it "single card is partial hand" $ do
            handScore (characterize [Card Ace Hearts]) `shouldBe` 11
            characterize [Card Ace Hearts] `shouldHaveWitness` SoftWitness

        it "ace adjustment with multiple cards" $ do
            handScore (characterize [Card Ace Hearts, Card Ace Spades, Card Ace Clubs, Card Ace Diamonds, Card Seven Hearts]) `shouldBe` 21

shouldHaveWitness :: SomeHand -> ValueWitness -> Expectation
shouldHaveWitness someHand expectedWitness =
    case valueType (handWitness someHand) of
        actualWitness | sameValueWitness expectedWitness actualWitness -> pure ()
        actualWitness ->
            expectationFailure $
                "Expected " ++ showValueWitness expectedWitness ++ " but got " ++ showValueWitness actualWitness

shouldHaveStructure :: SomeHand -> StructureWitness -> Expectation
shouldHaveStructure someHand expectedStructure =
    case structure (handWitness someHand) of
        actualStructure | sameStructureWitness expectedStructure actualStructure -> pure ()
        actualStructure ->
            expectationFailure $
                "Expected " ++ showStructureWitness expectedStructure ++ " but got " ++ showStructureWitness actualStructure

sameValueWitness :: ValueWitness -> ValueWitness -> Bool
sameValueWitness BlackjackWitness BlackjackWitness = True
sameValueWitness SoftWitness SoftWitness = True
sameValueWitness HardWitness HardWitness = True
sameValueWitness BustWitness BustWitness = True
sameValueWitness _ _ = False

sameStructureWitness :: StructureWitness -> StructureWitness -> Bool
sameStructureWitness (PairWitness r1) (PairWitness r2) = r1 == r2
sameStructureWitness EmptyWitness EmptyWitness = True
sameStructureWitness SingletonWitness SingletonWitness = True
sameStructureWitness NonPairWitness NonPairWitness = True
sameStructureWitness _ _ = False

showValueWitness :: ValueWitness -> String
showValueWitness BlackjackWitness = "BlackjackWitness"
showValueWitness SoftWitness = "SoftWitness"
showValueWitness HardWitness = "HardWitness"
showValueWitness BustWitness = "BustWitness"

showStructureWitness :: StructureWitness -> String
showStructureWitness (PairWitness r) = "PairWitness " ++ show r
showStructureWitness EmptyWitness = "EmptyWitness"
showStructureWitness SingletonWitness = "SingletonWitness"
showStructureWitness NonPairWitness = "NonPairWitness"
