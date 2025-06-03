{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.PlayerSurrenderSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Simulation hiding (Push)
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Player Surrender Scenario" $ describe "Event-Driven Surrender" $ do
    it "simulates player surrendering 16 vs dealer ace" $ do
        let gameEvents = playerSurrenderEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHaveBoutPlayerHandScore` testBoutId $ 16
        state `shouldHaveDealerUpcardScore` testBoutId $ 11
        verifySurrenderOutcome state

    it "verifies surrender results in half bet loss" $ do
        let gameEvents = playerSurrenderEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        verifySurrenderOutcome state

playerSurrenderEvents :: [SimulationEvent]
playerSurrenderEvents =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Ten Hearts)
           , dealerDealtCard (Card Ace Spades)
           , playerDealtCard (Card Six Diamonds)
           , dealerDealtCard (Card Queen Clubs)
           , playerSurrenders
           , gameEvent (BoutSettled testBoutId surrenderOutcome)
           ]

verifySurrenderOutcome :: SimState -> Expectation
verifySurrenderOutcome state =
    case withSimCache state $ deref testBoutId of
        Just bout -> do
            handScore (_bAttrsPlayerHand (_bAttrs bout)) `shouldBe` 16
            case _bAttrsOutcome (_bAttrs bout) of
                Present outcome -> outcome `shouldSatisfy` isSurrenderOutcome
                Absent -> expectationFailure "Expected bout outcome to be present"
        Nothing -> expectationFailure "Could not retrieve bout from cache"
  where
    isSurrenderOutcome outcome = case outcome of
        DetailedOutcome BoutDealerWin _ -> True
        _ -> False

surrenderOutcome :: DetailedOutcome
surrenderOutcome = DetailedOutcome BoutDealerWin Nothing
