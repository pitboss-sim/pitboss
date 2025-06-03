{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.BoutFlowSpec where

import Test.Hspec

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Bout Flow Integration" $ describe "card dealing and hand progression" $ do
    it "handles contestant stand after dealing cards" $ do
        let state =
                mkInitialState
                    |> dealCard (Card Ten Hearts)
                    |> dealCard (Card Seven Spades)
                    |> contestantStands

        state `shouldHaveContestantHandFSM` CHResolvedFSM CStand
        state `shouldHaveHandScore` 17

    it "maintains decision state after dealing two cards" $ do
        let state =
                mkInitialState
                    |> dealCard (Card Ten Hearts)
                    |> dealCard (Card Seven Spades)

        state `shouldHaveContestantHandFSM` CHDecisionFSM
        state `shouldHaveHandScore` 17

    it "transitions from initial state to resolved on stand" $ do
        let state = mkInitialState |> contestantStands

        state `shouldHaveContestantHandFSM` CHResolvedFSM CStand

    it "handles contestant hit resulting in bust" $ do
        let state =
                mkInitialState
                    |> dealCard (Card Ten Hearts)
                    |> dealCard (Card Seven Spades)
                    |> contestantHits
                    |> dealCard (Card King Clubs)
        state `shouldHaveContestantHandFSM` CHResolvedFSM CBust
        state `shouldHaveHandScore` 0

-- Local test helpers
(|>) :: SimState -> (SimState -> SimState) -> SimState
state |> action = action state

mkInitialState :: SimState
mkInitialState =
    let startTick = Tick 1000
        trace = mkInitialTrace startTick
     in SimState trace (EventLog mempty) (IntentLog mempty) startTick

dealCard :: Card -> SimState -> SimState
dealCard card = runEvent (CardDealt card testHandId)

contestantStands :: SimState -> SimState
contestantStands = runEvent (ContestantStood testContestantId testHandId)

contestantHits :: SimState -> SimState
contestantHits = runEvent (ContestantHit testContestantId testHandId)

shouldHaveContestantHandFSM :: SimState -> ContestantHandFSM p h d s -> Expectation
shouldHaveContestantHandFSM state expectedFSM =
    case withSimCache state $ deref testHandId of
        Just hand ->
            case _hModesHandFSM (_hModes hand) of
                ContestantHandFSM (SomeContestantHandFSM actualFSM) ->
                    case (expectedFSM, actualFSM) of
                        (CHDecisionFSM, CHDecisionFSM) -> pure ()
                        (CHResolvedFSM CStand, CHResolvedFSM CStand) -> pure ()
                        (CHResolvedFSM CBust, CHResolvedFSM CBust) -> pure ()
                        _ -> expectationFailure $ "Expected " ++ show expectedFSM ++ " but got " ++ show actualFSM
                other -> expectationFailure $ "Expected contestant hand FSM but got: " ++ show other
        Nothing -> expectationFailure "Hand not found in cache"

shouldHaveHandScore :: SimState -> Int -> Expectation
shouldHaveHandScore state expectedScore =
    case withSimCache state $ deref testHandId of
        Just hand -> handScore (_hAttrsHand (_hAttrs hand)) `shouldBe` expectedScore
        Nothing -> expectationFailure "Hand not found in cache"
