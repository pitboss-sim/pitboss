{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Spec.Pitboss.Integration.BoutFlowSpec where

import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Blackjack
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Spec.Pitboss.Helpers
import Test.Hspec

mkInitialSimState :: SimState
mkInitialSimState =
    let startTick = Tick 1000
        trace = mkInitialTrace startTick
     in SimState
            { simTrace = trace
            , simEventLog = EventLog IHM.empty
            , simIntentLog = IntentLog IHM.empty
            , simTick = startTick
            }

spec :: Spec
spec = describe "Bout Flow Integration" $ do
    it "processes a player stand event" $ do
        let state0 = mkInitialSimState
            playerId = EntityId 100 :: EntityId 'Player
            playerHandId = EntityId 400 :: EntityId 'PlayerHand

        let state1 = runEvent (PlayerStood playerId playerHandId) state0

        let result = withSimCache state1 $ do
                deref playerHandId

        case result of
            Just hand ->
                _phFsm (_phModes hand) `shouldBe` SomePlayerHandFSM (PHResolvedFSM PHStand)
            Nothing ->
                expectationFailure "Player hand not found"

    it "processes card dealing" $ do
        let state0 = mkInitialSimState
            playerHandId = EntityId 400 :: EntityId 'PlayerHand
            card = Card King Hearts

        let state1 = runEvent (CardDealt card (ToPlayerHand playerHandId)) state0

        let EventLog eventMap = simEventLog state1
            tick = simTick state0
            events = IHM.lookup tick eventMap

        case events of
            Just [event] -> eventOccurred event `shouldBe` CardDealt card (ToPlayerHand playerHandId)
            _ -> expectationFailure "Event not logged correctly"

    it "simulates a complete blackjack hand" $ do
        let state0 = mkInitialSimState
            playerId = EntityId 100 :: EntityId 'Player
            playerHandId = EntityId 400 :: EntityId 'PlayerHand
            dealerHandId = EntityId 500 :: EntityId 'DealerHand

        let state1 = runEvent (CardDealt (Card Ten Hearts) (ToPlayerHand playerHandId)) state0
            state2 = runEvent (CardDealt (Card Six Diamonds) (ToDealerHand dealerHandId)) state1
            state3 = runEvent (CardDealt (Card Seven Spades) (ToPlayerHand playerHandId)) state2
            state4 = runEvent (CardDealt (Card King Clubs) (ToDealerHand dealerHandId)) state3

        let playerHandAfterDeal = withSimCache state4 $ deref playerHandId
        case playerHandAfterDeal of
            Just hand -> do
                handScore (_phAttrsHand (_phAttrs hand)) `shouldBe` 17
                _phFsm (_phModes hand) `shouldBe` SomePlayerHandFSM PHDecisionFSM
            Nothing -> expectationFailure "Player hand not found after dealing"

        let state5 = runEvent (PlayerStood playerId playerHandId) state4

        let playerHandAfterStand = withSimCache state5 $ deref playerHandId
        case playerHandAfterStand of
            Just hand ->
                _phFsm (_phModes hand) `shouldBe` SomePlayerHandFSM (PHResolvedFSM PHStand)
            Nothing -> expectationFailure "Player hand not found after stand"

    -- TODO: Add dealer play sequence (dealer hits on 16, gets 4, stands on 20)
    -- TODO: Add bout settlement (dealer wins 20 vs 17)

    it "handles player blackjack" $ do
        let state0 = mkInitialSimState
            playerHandId = EntityId 400 :: EntityId 'PlayerHand
            dealerHandId = EntityId 500 :: EntityId 'DealerHand

        let state1 = runEvent (CardDealt (Card Ace Hearts) (ToPlayerHand playerHandId)) state0
            state2 = runEvent (CardDealt (Card Nine Diamonds) (ToDealerHand dealerHandId)) state1
            state3 = runEvent (CardDealt (Card King Spades) (ToPlayerHand playerHandId)) state2

        let playerHand = withSimCache state3 $ deref playerHandId
        case playerHand of
            Just hand -> do
                case _phAttrsHand (_phAttrs hand) of
                    SomeHand h -> case witness h of
                        BlackjackWitness -> pure ()
                        other -> expectationFailure $ "Expected blackjack but got: " ++ show other
            Nothing -> expectationFailure "Player hand not found"

    it "handles player bust" $ do
        let state0 = mkInitialSimState
            playerId = EntityId 100 :: EntityId 'Player
            playerHandId = EntityId 400 :: EntityId 'PlayerHand

        let state1 = runEvent (CardDealt (Card Ten Hearts) (ToPlayerHand playerHandId)) state0
            state2 = runEvent (CardDealt (Card Six Spades) (ToPlayerHand playerHandId)) state1

        let state3 = runEvent (PlayerHit playerId playerHandId) state2
            state4 = runEvent (CardDealt (Card King Clubs) (ToPlayerHand playerHandId)) state3

        let playerHand = withSimCache state4 $ deref playerHandId
        case playerHand of
            Just hand -> do
                case _phAttrsHand (_phAttrs hand) of
                    SomeHand h -> case witness h of
                        BustWitness -> pure ()
                        other -> expectationFailure $ "Expected bust but got: " ++ show other
            Nothing -> expectationFailure "Player hand not found"
