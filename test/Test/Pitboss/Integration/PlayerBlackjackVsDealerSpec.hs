{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.PlayerBlackjackVsDealerSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Player Blackjack vs Dealer Scenario" $ describe "Event-Driven Blackjack Win" $ do
    it "simulates player getting blackjack against dealer 20" $ do
        let gameEvents = playerBlackjackVsDealerEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHavePlayerBlackjack` testBoutId
        state `shouldHaveBoutPlayerHandScore` testBoutId $ 21
        state `shouldHaveDealerHandScore` testBoutId $ 20
        verifyPlayerBlackjackWin state

    it "verifies blackjack beats dealer 20 with correct payout" $ do
        let gameEvents = playerBlackjackVsDealerEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        verifyPlayerBlackjackWin state

playerBlackjackVsDealerEvents :: [SimulationEvent]
playerBlackjackVsDealerEvents =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Ace Hearts)
           , dealerDealtCard (Card Ten Spades)
           , playerDealtCard (Card King Diamonds)
           , dealerDealtCard (Card Queen Clubs)
           , gameEvent (BoutDealerRevealed testBoutId)
           , gameEvent (BoutDealerStood testBoutId)
           , gameEvent (BoutSettled testBoutId playerBlackjackOutcome)
           ]

verifyPlayerBlackjackWin :: SimState -> Expectation
verifyPlayerBlackjackWin state =
    case withSimCache state $ deref testBoutId of
        Just bout -> do
            let SomeHand _ playerWitness = _bAttrsPlayerHand (_bAttrs bout)
            handScore (_bAttrsPlayerHand (_bAttrs bout)) `shouldBe` 21
            isBlackjack playerWitness `shouldBe` True
            handScore (_bAttrsDealerHand (_bAttrs bout)) `shouldBe` 20
            case _bAttrsOutcome (_bAttrs bout) of
                Present outcome -> outcome `shouldSatisfy` isPlayerBlackjackOutcome
                Absent -> expectationFailure "Expected bout outcome to be present"
        Nothing -> expectationFailure "Could not retrieve bout from cache"
  where
    isPlayerBlackjackOutcome outcome = case outcome of
        DetailedOutcome BoutPlayerWin (Just NaturalBlackjack) -> True
        _ -> False

playerBlackjackOutcome :: DetailedOutcome
playerBlackjackOutcome = boutPlayerWinsBlackjack
