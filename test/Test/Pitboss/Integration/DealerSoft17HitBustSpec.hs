{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.DealerSoft17HitBustSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Dealer Bust Scenario" $ describe "Event-Driven Dealer Bust" $ do
    it "simulates dealer hitting and busting with multiple cards" $ do
        let gameEvents = dealerSoft17BustEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHaveBoutPlayerHandScore` testBoutId $ 18
        state `shouldHaveDealerHandScore` testBoutId $ 23
        verifyDealerBustOutcome state

    it "verifies dealer bust leads to player win" $ do
        let gameEvents = dealerSoft17BustEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        verifyDealerBustOutcome state

dealerSoft17BustEvents :: [SimulationEvent]
dealerSoft17BustEvents =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Nine Hearts)
           , dealerDealtCard (Card Six Spades)
           , playerDealtCard (Card Nine Diamonds)
           , dealerDealtCard (Card Six Clubs)
           , playerStands
           , gameEvent (BoutDealerRevealed testBoutId)
           , dealerDealtCard (Card Six Hearts)
           , dealerDealtCard (Card Five Hearts)
           , gameEvent (BoutSettled testBoutId dealerBustOutcome)
           ]

verifyDealerBustOutcome :: SimState -> Expectation
verifyDealerBustOutcome state =
    case withSimCache state $ deref testBoutId of
        Just bout -> do
            let SomeHand _ dealerWitness = _bAttrsDealerHand (_bAttrs bout)
            handScore (_bAttrsPlayerHand (_bAttrs bout)) `shouldBe` 18
            handScore (_bAttrsDealerHand (_bAttrs bout)) `shouldBe` 23
            isBust dealerWitness `shouldBe` True
            case _bAttrsOutcome (_bAttrs bout) of
                Present outcome -> outcome `shouldSatisfy` isPlayerWinDealerBust
                Absent -> expectationFailure "Expected bout outcome to be present"
        Nothing -> expectationFailure "Could not retrieve bout from cache"
  where
    isPlayerWinDealerBust outcome = case outcome of
        DetailedOutcome BoutPlayerWin (Just OpponentBust) -> True
        _ -> False

dealerBustOutcome :: DetailedOutcome
dealerBustOutcome = boutPlayerWinsDealerBust
