{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.PlayerSplitPairsSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Simulation hiding (Push)
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Player Split Pairs Scenario" $ describe "Event-Driven Pair Split" $ do
    it "simulates player splitting 8s vs dealer 6" $ do
        let gameEvents = playerSplitPairsEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHaveActiveSplitHands` 2
        verifySplitPairsBasics state

    it "verifies split pairs creates two hands from original pair" $ do
        let gameEvents = playerSplitPairsEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        verifySplitPairsBasics state

playerSplitPairsEvents :: [SimulationEvent]
playerSplitPairsEvents =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Eight Hearts)
           , dealerDealtCard (Card Six Spades)
           , playerDealtCard (Card Eight Diamonds)
           , dealerDealtCard (Card Queen Clubs)
           , playerSplits
           ]

verifySplitPairsBasics :: SimState -> Expectation
verifySplitPairsBasics state =
    case withSimCache state $ deref testBoutId of
        Just bout -> do
            case _bAttrsPlayerHand (_bAttrs bout) of
                SomeHand cards _ ->
                    case cards of
                        [Card Eight _] -> pure ()
                        _ -> expectationFailure $ "Expected single eight after split, got " ++ show cards
        Nothing -> expectationFailure "Could not retrieve bout from cache"
