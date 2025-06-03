{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.ComplexPushVariationsSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Simulation hiding (Push)
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Complex Push Variations Scenario" $ describe "Event-Driven Push Outcomes" $ do
    it "simulates player and dealer both reaching 19" $ do
        let gameEvents = push19Events
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHaveBoutPlayerHandScore` testBoutId $ 19
        state `shouldHaveDealerHandScore` testBoutId $ 19
        verifyPushOutcome state

    it "simulates player and dealer both reaching 20" $ do
        let gameEvents = push20Events
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHaveBoutPlayerHandScore` testBoutId $ 20
        state `shouldHaveDealerHandScore` testBoutId $ 20
        verifyPushOutcome state

    it "simulates player and dealer both reaching 21 (non-blackjack)" $ do
        let gameEvents = push21Events
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHaveBoutPlayerHandScore` testBoutId $ 21
        state `shouldHaveDealerHandScore` testBoutId $ 21
        verifyPushOutcome state

push19Events :: [SimulationEvent]
push19Events =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Ten Hearts)
           , dealerDealtCard (Card Nine Spades)
           , playerDealtCard (Card Nine Diamonds)
           , dealerDealtCard (Card Queen Clubs)
           , playerStands
           , gameEvent (BoutDealerRevealed testBoutId)
           , gameEvent (BoutDealerStood testBoutId)
           , gameEvent (BoutSettled testBoutId pushOutcome)
           ]

push20Events :: [SimulationEvent]
push20Events =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Ten Hearts)
           , dealerDealtCard (Card Ten Spades)
           , playerDealtCard (Card Queen Diamonds)
           , dealerDealtCard (Card King Clubs)
           , playerStands
           , gameEvent (BoutDealerRevealed testBoutId)
           , gameEvent (BoutDealerStood testBoutId)
           , gameEvent (BoutSettled testBoutId pushOutcome)
           ]

push21Events :: [SimulationEvent]
push21Events =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Seven Hearts)
           , dealerDealtCard (Card Seven Spades)
           , playerDealtCard (Card Seven Diamonds)
           , dealerDealtCard (Card Seven Clubs)
           , playerDealtCard (Card Seven Hearts)
           , dealerDealtCard (Card Seven Diamonds)
           , playerStands
           , gameEvent (BoutDealerRevealed testBoutId)
           , gameEvent (BoutDealerStood testBoutId)
           , gameEvent (BoutSettled testBoutId pushOutcome)
           ]

verifyPushOutcome :: SimState -> Expectation
verifyPushOutcome state =
    case withSimCache state $ deref testBoutId of
        Just bout -> do
            let playerScore = handScore (_bAttrsPlayerHand (_bAttrs bout))
                dealerScore = handScore (_bAttrsDealerHand (_bAttrs bout))
            playerScore `shouldBe` dealerScore
            case _bAttrsOutcome (_bAttrs bout) of
                Present outcome -> outcome `shouldSatisfy` isPushOutcome
                Absent -> expectationFailure "Expected bout outcome to be present"
        Nothing -> expectationFailure "Could not retrieve bout from cache"
  where
    isPushOutcome outcome = case outcome of
        DetailedOutcome Push Nothing -> True
        _ -> False
