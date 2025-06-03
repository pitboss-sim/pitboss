{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Integration.DealerBlackjackSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Dealer Blackjack Integration" $ do
    describe "Dealer Natural Blackjack" $ do
        it "dealer gets blackjack with Ace and Ten" $ do
            let state = mkBlackjackScenario vegas6 [Card Ten Hearts, Card Nine Spades] [Card Ace Hearts, Card King Spades]
                revealState = runEvent (BoutDealerRevealed testBoutId) state

            shouldHaveDealerBlackjack revealState testBoutId
            shouldHaveBoutPlayerHandScore revealState testBoutId 19

        it "dealer gets blackjack with Ten and Ace" $ do
            let state = mkBlackjackScenario vegas6 [Card Seven Hearts, Card Eight Spades] [Card King Hearts, Card Ace Spades]
                revealState = runEvent (BoutDealerRevealed testBoutId) state

            shouldHaveDealerBlackjack revealState testBoutId
            shouldHaveBoutPlayerHandScore revealState testBoutId 15

        it "dealer blackjack beats player 21" $ do
            let state = mkBlackjackScenario vegas6 [Card Seven Hearts, Card Seven Spades, Card Seven Clubs] [Card Ace Hearts, Card Queen Spades]
                revealState = runEvent (BoutDealerRevealed testBoutId) state

            shouldHaveDealerBlackjack revealState testBoutId
            shouldHaveBoutPlayerHandScore revealState testBoutId 21

    describe "Dealer Blackjack vs Player Blackjack" $ do
        it "pushes when both have blackjack" $ do
            let state = mkBlackjackScenario vegas6 [Card Ace Hearts, Card King Spades] [Card Ace Spades, Card Queen Hearts]
                revealState = runEvent (BoutDealerRevealed testBoutId) state

            shouldHaveDealerBlackjack revealState testBoutId
            shouldHavePlayerBlackjack revealState testBoutId

    describe "Non-Blackjack Dealer Hands" $ do
        it "dealer does not have blackjack with soft 21" $ do
            let state = mkBlackjackScenario vegas6 [Card Ten Hearts, Card Nine Spades] [Card Ace Hearts, Card Five Spades, Card Five Clubs]
                revealState = runEvent (BoutDealerRevealed testBoutId) state

            shouldNotHaveDealerBlackjack revealState testBoutId
            shouldHaveDealerHandScore revealState testBoutId 21

        it "dealer does not have blackjack with hard 21" $ do
            let state = mkBlackjackScenario vegas6 [Card Ten Hearts, Card Nine Spades] [Card Seven Hearts, Card Seven Spades, Card Seven Clubs]
                revealState = runEvent (BoutDealerRevealed testBoutId) state

            shouldNotHaveDealerBlackjack revealState testBoutId
            shouldHaveDealerHandScore revealState testBoutId 21

mkBlackjackScenario :: Offering -> [Card] -> [Card] -> SimState
mkBlackjackScenario offering playerCards dealerCards =
    buildSimStateFromMixedEvents
        [ lifecycleEvent (playerCreated testPlayerId "Test Player")
        , lifecycleEvent (dealerCreated testDealerId "Test Dealer")
        , lifecycleEvent (tableCreated testTableId "Test Table" offering)
        , lifecycleEvent (roundCreated testRoundId testShoeId testTableId)
        , lifecycleEvent (shoeCreated testShoeId testTableId [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts])
        , lifecycleEvent (boutCreated testBoutId testPlayerId testDealerId testRoundId testTableId)
        , playerHandSet playerCards
        , dealerHandSet dealerCards
        ]
        testTick
