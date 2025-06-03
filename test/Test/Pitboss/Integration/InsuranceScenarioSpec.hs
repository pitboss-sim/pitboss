{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.InsuranceScenarioSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Simulation hiding (Push)
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Insurance Scenario" $ describe "Event-Driven Insurance Mechanics" $ do
    it "simulates basic insurance scenario setup" $ do
        let gameEvents = insuranceScenarioEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHaveBoutPlayerHandScore` testBoutId $ 20
        state `shouldHaveDealerUpcard` testBoutId $ Ace
        verifyInsuranceSetup state

    it "verifies dealer ace upcard triggers insurance possibility" $ do
        let gameEvents = insuranceScenarioEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        verifyInsuranceSetup state

insuranceScenarioEvents :: [SimulationEvent]
insuranceScenarioEvents =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Ten Hearts)
           , dealerDealtCard (Card Ace Spades)
           , playerDealtCard (Card Queen Diamonds)
           , dealerDealtCard (Card King Clubs)
           , gameEvent (BoutDealerRevealed testBoutId)
           , gameEvent (BoutSettled testBoutId dealerWinsBlackjack)
           ]

verifyInsuranceSetup :: SimState -> Expectation
verifyInsuranceSetup state =
    case withSimCache state $ deref testBoutId of
        Just bout -> do
            let SomeHand _ dealerWitness = _bAttrsDealerHand (_bAttrs bout)
            handScore (_bAttrsPlayerHand (_bAttrs bout)) `shouldBe` 20
            handScore (_bAttrsDealerHand (_bAttrs bout)) `shouldBe` 21
            isBlackjack dealerWitness `shouldBe` True
            case _bAttrsOutcome (_bAttrs bout) of
                Present outcome -> outcome `shouldSatisfy` isDealerBlackjackWin
                Absent -> expectationFailure "Expected bout outcome to be present"
        Nothing -> expectationFailure "Could not retrieve bout from cache"
  where
    isDealerBlackjackWin outcome = case outcome of
        DetailedOutcome BoutDealerWin (Just NaturalBlackjack) -> True
        _ -> False
