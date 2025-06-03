{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.PlayerDoubleDownWinSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Player Double Down Win Scenario" $ describe "Event-Driven Double Down" $ do
    it "simulates player doubling down on 11, getting 10, beating dealer 19" $ do
        let gameEvents = playerDoubleDownWinEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHavePlayerHandFSM` testBoutId $ PHResolvedFSM PHStand
        state `shouldHaveBoutPlayerHandScore` testBoutId $ 21
        state `shouldHaveDealerHandScore` testBoutId $ 19
        verifyDoubleDownWinOutcome state

    it "verifies double down bet mechanics and outcome" $ do
        let gameEvents = playerDoubleDownWinEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        verifyDoubleDownWinOutcome state

playerDoubleDownWinEvents :: [SimulationEvent]
playerDoubleDownWinEvents =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Five Hearts)
           , dealerDealtCard (Card Nine Spades)
           , playerDealtCard (Card Six Diamonds)
           , dealerDealtCard (Card Queen Clubs)
           , playerDoubles
           , playerDealtCard (Card Ten Hearts)
           , playerStands
           , gameEvent (BoutDealerRevealed testBoutId)
           , gameEvent (BoutDealerStood testBoutId)
           , gameEvent (BoutSettled testBoutId playerWinsDoubleDown)
           ]

verifyDoubleDownWinOutcome :: SimState -> Expectation
verifyDoubleDownWinOutcome state =
    case withSimCache state $ deref testBoutId of
        Just bout -> do
            handScore (_bAttrsPlayerHand (_bAttrs bout)) `shouldBe` 21
            handScore (_bAttrsDealerHand (_bAttrs bout)) `shouldBe` 19
            case _bAttrsOutcome (_bAttrs bout) of
                Present outcome -> outcome `shouldSatisfy` isPlayerWinOutcome
                Absent -> expectationFailure "Expected bout outcome to be present"
        Nothing -> expectationFailure "Could not retrieve bout from cache"
  where
    isPlayerWinOutcome outcome = case outcome of
        DetailedOutcome BoutPlayerWin _ -> True
        _ -> False

playerWinsDoubleDown :: DetailedOutcome
playerWinsDoubleDown = boutPlayerWinsHigher
