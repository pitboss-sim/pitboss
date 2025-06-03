{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.BoutFlowSpec where

import Test.Hspec

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Bout Flow Integration" $ describe "card dealing and hand progression" $ do
    it "handles boutPlayer stand after dealing cards" $ do
        let state =
                buildSimStateFromMixedEvents
                    [ lifecycleEvent (playerCreated testPlayerId "Test Player")
                    , lifecycleEvent (dealerCreated testDealerId "Test Dealer")
                    , lifecycleEvent (tableCreated testTableId "Test Table" vegas6)
                    , lifecycleEvent (roundCreated testRoundId testShoeId testTableId)
                    , lifecycleEvent (shoeCreated testShoeId testTableId [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts])
                    , lifecycleEvent (boutCreated testBoutId testPlayerId testDealerId testRoundId testTableId)
                    , playerHandSet [Card Ten Hearts, Card Seven Spades]
                    , gameEvent (BoutPlayerStood testBoutId)
                    ]
                    (Tick 1000)

        shouldHavePlayerHandFSM state testBoutId (PHResolvedFSM PHStand)
        shouldHaveBoutPlayerHandScore state testBoutId 17

    it "maintains decision state after dealing two cards" $ do
        let state =
                buildSimStateFromMixedEvents
                    [ lifecycleEvent (playerCreated testPlayerId "Test Player")
                    , lifecycleEvent (dealerCreated testDealerId "Test Dealer")
                    , lifecycleEvent (tableCreated testTableId "Test Table" vegas6)
                    , lifecycleEvent (roundCreated testRoundId testShoeId testTableId)
                    , lifecycleEvent (shoeCreated testShoeId testTableId [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts])
                    , lifecycleEvent (boutCreated testBoutId testPlayerId testDealerId testRoundId testTableId)
                    , playerHandSet [Card Ten Hearts, Card Seven Spades]
                    ]
                    (Tick 1000)

        shouldHavePlayerHandFSM state testBoutId PHDecisionFSM
        shouldHaveBoutPlayerHandScore state testBoutId 17

    it "transitions from initial state to resolved on stand" $ do
        let state =
                buildSimStateFromMixedEvents
                    [ lifecycleEvent (playerCreated testPlayerId "Test Player")
                    , lifecycleEvent (dealerCreated testDealerId "Test Dealer")
                    , lifecycleEvent (tableCreated testTableId "Test Table" vegas6)
                    , lifecycleEvent (roundCreated testRoundId testShoeId testTableId)
                    , lifecycleEvent (shoeCreated testShoeId testTableId [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts])
                    , lifecycleEvent (boutCreated testBoutId testPlayerId testDealerId testRoundId testTableId)
                    , gameEvent (BoutPlayerStood testBoutId)
                    ]
                    (Tick 1000)

        shouldHavePlayerHandFSM state testBoutId (PHResolvedFSM PHStand)

    it "handles boutPlayer hit resulting in bust" $ do
        let state =
                buildSimStateFromMixedEvents
                    [ lifecycleEvent (playerCreated testPlayerId "Test Player")
                    , lifecycleEvent (dealerCreated testDealerId "Test Dealer")
                    , lifecycleEvent (tableCreated testTableId "Test Table" vegas6)
                    , lifecycleEvent (roundCreated testRoundId testShoeId testTableId)
                    , lifecycleEvent (shoeCreated testShoeId testTableId [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts])
                    , lifecycleEvent (boutCreated testBoutId testPlayerId testDealerId testRoundId testTableId)
                    , playerHandSet [Card Ten Hearts, Card Seven Spades]
                    , gameEvent (BoutPlayerHit testBoutId)
                    , gameEvent (BoutPlayerCardDealt (Card King Clubs) testBoutId)
                    ]
                    (Tick 1000)
        shouldHavePlayerHandFSM state testBoutId (PHResolvedFSM PHBust)
        shouldHaveBoutPlayerHandScore state testBoutId 27
