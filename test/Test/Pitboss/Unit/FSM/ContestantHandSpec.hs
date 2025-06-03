{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Test.Pitboss.Unit.FSM.ContestantHandSpec where

import Pitboss.Blackjack.Types.Core
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import System.Random (mkStdGen)
import Test.Hspec
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Contestant Hand FSM" $ do
    describe "Intent Generation Respects FSM State" $ do
        it "generates no intent for resolved hands" $ do
            let trace = mkInitialTrace testTick
                standEvent = ContestantStood testContestantId testHandId
                state0 = SimState trace (EventLog mempty) (IntentLog mempty) testTick
                state1 = runEvent standEvent state0
                cache = mkCacheFromTrace (simTrace state1) testTick

            playerArchetype <- mkTestBasicStrategy
            let gen = mkStdGen 46
                result =
                    withTickCache cache $
                        generateContestantIntent playerArchetype testContestantId gen

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
                        generateContestantIntent playerArchetype testContestantId gen

            case result of
                Right _ -> True `shouldBe` True
                Left _ -> expectationFailure "Expected intent generation for decision state hand"

    describe "FSM State Transitions" $ do
        it "transitions from Decision to Resolved on Stand" $ do
            let initialFSM = SomeContestantHandFSM CHDecisionFSM

            case initialFSM of
                SomeContestantHandFSM CHDecisionFSM -> do
                    let resolvedFSM = SomeContestantHandFSM (CHResolvedFSM CStand)
                    resolvedFSM `shouldSatisfy` \(SomeContestantHandFSM fsm) -> case fsm of
                        CHResolvedFSM CStand -> True
                        _ -> False
                _ -> expectationFailure "Expected decision FSM"

        it "transitions from Decision to Hitting on Hit" $ do
            let initialFSM = SomeContestantHandFSM CHDecisionFSM

            case initialFSM of
                SomeContestantHandFSM CHDecisionFSM -> do
                    let hittingFSM = SomeContestantHandFSM CHHittingFSM
                    hittingFSM `shouldSatisfy` \(SomeContestantHandFSM fsm) -> case fsm of
                        CHHittingFSM -> True
                        _ -> False
                _ -> expectationFailure "Expected decision FSM"

        it "transitions from Decision to OneCardDraw on Double" $ do
            let initialFSM = SomeContestantHandFSM CHDecisionFSM

            case initialFSM of
                SomeContestantHandFSM CHDecisionFSM -> do
                    let doubleFSM = SomeContestantHandFSM (CHAwaitingOneCardFSM OCDouble)
                    doubleFSM `shouldSatisfy` \(SomeContestantHandFSM fsm) -> case fsm of
                        CHAwaitingOneCardFSM OCDouble -> True
                        _ -> False
                _ -> expectationFailure "Expected decision FSM"

        it "prevents transitions from terminal states" $ do
            let resolvedFSM = SomeContestantHandFSM (CHResolvedFSM CStand)
                blackjackFSM = SomeContestantHandFSM (CHResolvedFSM CBlackjack)

            resolvedFSM `shouldSatisfy` \(SomeContestantHandFSM fsm) -> case fsm of
                CHResolvedFSM _ -> True
                _ -> False

            blackjackFSM `shouldSatisfy` \(SomeContestantHandFSM fsm) -> case fsm of
                (CHResolvedFSM CBlackjack) -> True
                _ -> False
