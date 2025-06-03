{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.MultiSplitScenarioSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Simulation hiding (Push)
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Multi-Split Scenario" $ describe "Event-Driven Split Mechanics" $ do
    it "demonstrates basic split functionality with pairs" $ do
        let gameEvents = basicSplitEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHaveActiveSplitHands` 2
        verifyBasicSplitSetup state

    it "verifies split creates two separate hands from pair" $ do
        let gameEvents = basicSplitEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        verifyBasicSplitSetup state

basicSplitEvents :: [SimulationEvent]
basicSplitEvents =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Two Hearts)
           , dealerDealtCard (Card Six Spades)
           , playerDealtCard (Card Two Diamonds)
           , dealerDealtCard (Card Queen Clubs)
           , playerSplits
           ]

verifyBasicSplitSetup :: SimState -> Expectation
verifyBasicSplitSetup state =
    case withSimCache state $ deref testBoutId of
        Just bout -> do
            case _bAttrsPlayerHand (_bAttrs bout) of
                SomeHand cards _ ->
                    case cards of
                        [Card Two _] -> pure ()
                        _ -> expectationFailure $ "Expected single two after split, got " ++ show cards
        Nothing -> expectationFailure "Could not retrieve bout from cache"
