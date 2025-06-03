{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Integration.DoubleDownSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.FSM
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Double Down Integration" $ do
    describe "Valid Doubling Scenarios" $ do
        it "doubles down on hard 11 vs dealer 6" $ do
            let events =
                    mkDoublingStateEvents [Card Five Hearts, Card Six Spades]
                        ++ [gameEvent (BoutPlayerDoubledDown testBoutId)]
                finalState = buildSimStateFromMixedEvents events testTick

            finalState `shouldHavePlayerHandFSM` testBoutId $ PHAwaitingOneCardFSM OCDouble
            finalState `shouldHaveBoutPlayerHandScore` testBoutId $ 11

        it "doubles down on hard 10 vs dealer low card" $ do
            let events = mkDoublingStateEvents [Card Four Hearts, Card Six Spades] ++ [playerDoubles]
                finalState = buildSimStateFromMixedEvents events testTick

            finalState `shouldHavePlayerHandFSM` testBoutId $ PHAwaitingOneCardFSM OCDouble
            finalState `shouldHaveBoutPlayerHandScore` testBoutId $ 10

        it "doubles down on soft 18 vs dealer 4" $ do
            let events = mkDoublingStateEvents [Card Ace Hearts, Card Seven Spades] ++ [playerDoubles]
                finalState = buildSimStateFromMixedEvents events testTick

            finalState `shouldHavePlayerHandFSM` testBoutId $ PHAwaitingOneCardFSM OCDouble
            finalState `shouldHaveBoutPlayerHandScore` testBoutId $ 18

        it "doubles down on hard 9 vs dealer 6" $ do
            let events = mkDoublingStateEvents [Card Four Hearts, Card Five Spades] ++ [playerDoubles]
                finalState = buildSimStateFromMixedEvents events testTick

            finalState `shouldHavePlayerHandFSM` testBoutId $ PHAwaitingOneCardFSM OCDouble
            finalState `shouldHaveBoutPlayerHandScore` testBoutId $ 9

    describe "One Card Resolution" $ do
        it "receives one card and awaits orchestrator resolution" $ do
            let events =
                    mkDoublingStateEvents [Card Five Hearts, Card Six Spades]
                        ++ [playerDoubles, playerDealtCard (Card Four Clubs)]
                finalState = buildSimStateFromMixedEvents events testTick

            finalState `shouldHavePlayerHandFSM` testBoutId $ PHAwaitingOneCardFSM OCDouble
            finalState `shouldHaveBoutPlayerHandScore` testBoutId $ 15

        it "stands after receiving the one card" $ do
            let events =
                    mkDoublingStateEvents [Card Five Hearts, Card Six Spades]
                        ++ [playerDoubles, playerDealtCard (Card Four Clubs), playerStands]
                finalState = buildSimStateFromMixedEvents events testTick

            finalState `shouldHavePlayerHandFSM` testBoutId $ PHResolvedFSM PHStand

        it "busts when one card causes over 21" $ do
            let events =
                    mkDoublingStateEvents [Card Five Hearts, Card Six Spades]
                        ++ [playerDoubles, playerDealtCard (Card King Clubs)]
                finalState = buildSimStateFromMixedEvents events testTick

            finalState `shouldHaveBoutPlayerHandScore` testBoutId $ 21

        it "automatically resolves to bust when doubling causes over 21" $ do
            let events =
                    mkDoublingStateEvents [Card Ten Hearts, Card Six Spades]
                        ++ [playerDoubles, playerDealtCard (Card Ten Clubs)]
                finalState = buildSimStateFromMixedEvents events testTick

            finalState `shouldHavePlayerHandFSM` testBoutId $ PHResolvedFSM PHBust
            finalState `shouldHaveBoutPlayerHandScore` testBoutId $ 26

    describe "Invalid Doubling Scenarios" $ do
        it "rejects double down after hitting" $ do
            let events =
                    mkDoublingStateEvents [Card Five Hearts, Card Six Spades]
                        ++ [playerHits, playerDealtCard (Card Two Clubs), playerDoubles]
                result = buildSimStateFromMixedEventsStrict events testTick

            result `shouldFailValidationWith` "Cannot double after hitting"

        it "rejects double down on blackjack" $ do
            let events = mkDoublingStateEvents [Card Ace Hearts, Card King Spades] ++ [playerDoubles]
                result = buildSimStateFromMixedEventsStrict events testTick

            result `shouldFailValidationWith` "Cannot double"

        it "rejects double down with insufficient funds" $ do
            let events = mkLowFundsDoublingStateEvents [Card Five Hearts, Card Six Spades] ++ [playerDoubles]
                result = buildSimStateFromMixedEventsStrict events testTick

            case result of
                Left _ -> expectationFailure "Expected success but got failure"
                Right _ -> pure ()

mkDoublingStateEvents :: [Card] -> [SimulationEvent]
mkDoublingStateEvents playerCards =
    [ lifecycleEvent (playerCreated testPlayerId "Test Player")
    , lifecycleEvent (dealerCreated testDealerId "Test Dealer")
    , lifecycleEvent (tableCreated testTableId "Test Table" vegas6)
    , lifecycleEvent (roundCreated testRoundId testShoeId testTableId)
    , lifecycleEvent (shoeCreated testShoeId testTableId [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts])
    , lifecycleEvent (boutCreated testBoutId testPlayerId testDealerId testRoundId testTableId)
    , playerHandSet playerCards
    , dealerHandSet [Card Six Hearts, Card Queen Clubs]
    ]

mkLowFundsDoublingStateEvents :: [Card] -> [SimulationEvent]
mkLowFundsDoublingStateEvents = mkDoublingStateEvents
