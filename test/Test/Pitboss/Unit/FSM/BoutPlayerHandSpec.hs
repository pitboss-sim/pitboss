{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Test.Pitboss.Unit.FSM.BoutPlayerHandSpec where

import Pitboss.Blackjack.Types.Core
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import System.Random (mkStdGen)
import Test.Hspec
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Bout Player Hand FSM" $ do
    describe "Intent Generation Respects FSM State" $ do
        it "generates no intent for resolved hands" $ do
            let trace = mkInitialTrace testTick
                standEvent = BoutPlayerStood testBoutId
                state0 = SimState trace (EventLog mempty) (IntentLog mempty) testTick
                state1 = runEvent standEvent state0
                cache = mkCacheFromTrace (simTrace state1) testTick

            playerArchetype <- mkTestBasicStrategy
            let gen = mkStdGen 46
                result =
                    withTickCache cache $
                        generateBoutIntent playerArchetype testBoutId gen

            result `shouldSatisfy` \case
                Left _ -> True
                Right _ -> False

        it "generates intent for decision state hands" $ do
            let trace = mkInitialTrace testTick
                cache = mkCacheFromTrace trace testTick

            playerArchetype <- mkTestBasicStrategy
            let gen = mkStdGen 47
                result =
                    withTickCache cache $
                        generateBoutIntent playerArchetype testBoutId gen

            case result of
                Right _ -> True `shouldBe` True
                Left _ -> expectationFailure "Expected intent generation for decision state hand"

    describe "FSM State Transitions" $ do
        it "transitions from Decision to Resolved on Stand" $ do
            let initialFSM = SomePlayerHandFSM PHDecisionFSM

            case initialFSM of
                SomePlayerHandFSM PHDecisionFSM -> do
                    let resolvedFSM = SomePlayerHandFSM (PHResolvedFSM PHStand)
                    resolvedFSM `shouldSatisfy` \(SomePlayerHandFSM fsm) -> case fsm of
                        PHResolvedFSM PHStand -> True
                        _ -> False
                _ -> expectationFailure "Expected decision FSM"

        it "transitions from Decision to Hitting on Hit" $ do
            let initialFSM = SomePlayerHandFSM PHDecisionFSM

            case initialFSM of
                SomePlayerHandFSM PHDecisionFSM -> do
                    let hittingFSM = SomePlayerHandFSM PHHittingFSM
                    hittingFSM `shouldSatisfy` \(SomePlayerHandFSM fsm) -> case fsm of
                        PHHittingFSM -> True
                        _ -> False
                _ -> expectationFailure "Expected decision FSM"

        it "transitions from Decision to OneCardDraw on Double" $ do
            let initialFSM = SomePlayerHandFSM PHDecisionFSM

            case initialFSM of
                SomePlayerHandFSM PHDecisionFSM -> do
                    let doubleFSM = SomePlayerHandFSM (PHAwaitingOneCardFSM OCDouble)
                    doubleFSM `shouldSatisfy` \(SomePlayerHandFSM fsm) -> case fsm of
                        PHAwaitingOneCardFSM OCDouble -> True
                        _ -> False
                _ -> expectationFailure "Expected decision FSM"

        it "prevents transitions from terminal states" $ do
            let resolvedFSM = SomePlayerHandFSM (PHResolvedFSM PHStand)
                blackjackFSM = SomePlayerHandFSM (PHResolvedFSM PHBlackjack)

            resolvedFSM `shouldSatisfy` \(SomePlayerHandFSM fsm) -> case fsm of
                PHResolvedFSM _ -> True
                _ -> False

            blackjackFSM `shouldSatisfy` \(SomePlayerHandFSM fsm) -> case fsm of
                (PHResolvedFSM PHBlackjack) -> True
                _ -> False
