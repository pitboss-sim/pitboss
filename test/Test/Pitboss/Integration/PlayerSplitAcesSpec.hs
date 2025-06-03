{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.PlayerSplitAcesSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Player Split Aces Scenario" $ describe "Event-Driven Ace Split" $ do
    it "simulates player splitting aces successfully" $ do
        let gameEvents = playerSplitAcesEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHaveActiveSplitHands` 2
        state `shouldHavePlayerHandFSM` testBoutId $ PHAwaitingOneCardFSM OCSplitAce
        verifySplitAcesBasics state

    it "verifies split aces enters one-card-only mode" $ do
        let gameEvents = playerSplitAcesEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHavePlayerHandFSM` testBoutId $ PHAwaitingOneCardFSM OCSplitAce

playerSplitAcesEvents :: [SimulationEvent]
playerSplitAcesEvents =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Ace Hearts)
           , dealerDealtCard (Card Seven Spades)
           , playerDealtCard (Card Ace Diamonds)
           , dealerDealtCard (Card Queen Clubs)
           , playerSplits
           ]

verifySplitAcesBasics :: SimState -> Expectation
verifySplitAcesBasics state =
    case withSimCache state $ deref testBoutId of
        Just bout -> do
            case _bAttrsPlayerHand (_bAttrs bout) of
                SomeHand cards _ ->
                    case cards of
                        [Card Ace _] -> pure ()
                        _ -> expectationFailure $ "Expected single ace after split, got " ++ show cards
        Nothing -> expectationFailure "Could not retrieve bout from cache"
