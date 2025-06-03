{-# LANGUAGE DataKinds #-}

module Test.Pitboss.Unit.Simulation.IntentValidationSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Causality.Validate
import Pitboss.FSM
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.TestUtils

mkValidationTestTrace :: Tick -> Trace
mkValidationTestTrace startTick =
    let
        contestantHand =
            mkTestHandWithFSM
                (ContestantOwner testContestantId)
                (characterize [Card Ten Hearts, Card Six Spades])
                (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM))

        bustedHand =
            mkTestHandWithFSM
                (ContestantOwner testContestantId)
                (characterize [Card Ten Hearts, Card Six Spades, Card King Clubs])
                (ContestantHandFSM (SomeContestantHandFSM CHHittingFSM))

        contestant = mkTestContestant testPlayerId testRoundId
        player = mkTestPlayer "Test Player"

        trace0 = emptyTrace
        trace1 = applyTraceOp (createBirth testHandId contestantHand) startTick trace0
        trace2 = applyTraceOp (createBirth (HandId 999) bustedHand) startTick trace1
        trace3 = applyTraceOp (createBirth testContestantId contestant) startTick trace2
        trace4 = applyTraceOp (createBirth testPlayerId player) startTick trace3
     in
        trace4

mkBlackjackTestTrace :: Tick -> Trace
mkBlackjackTestTrace startTick =
    let blackjackHand' =
            mkTestHandWithFSM
                (ContestantOwner testContestantId)
                (characterize [Card Ace Hearts, Card King Spades])
                (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM))

        contestant = mkTestContestant testPlayerId testRoundId

        trace0 = emptyTrace
        trace1 = applyTraceOp (createBirth testHandId blackjackHand') startTick trace0
        trace2 = applyTraceOp (createBirth testContestantId contestant) startTick trace1
     in trace2

convertValidationResult :: Validation ValidationErrors Bool -> Either String ()
convertValidationResult (Success True) = Right ()
convertValidationResult (Success False) = Left "Validation failed"
convertValidationResult (Failure (ValidationErrors errors)) =
    Left $ "Validation errors: " ++ show errors

testValidation :: (Validateable k) => Trace -> Tick -> IntentCtx k -> Either String () -> Expectation
testValidation trace tick ctx expected =
    let cache = mkCacheFromTrace trace tick
        result = withTickCache cache (validate ctx)
        converted = convertValidationResult result
     in converted `shouldBe` expected

testPlayerHitValidation :: Trace -> Tick -> IntentCtx 'IPlayerHit -> Either String () -> Expectation
testPlayerHitValidation = testValidation

testPlayerStandValidation :: Trace -> Tick -> IntentCtx 'IPlayerStand -> Either String () -> Expectation
testPlayerStandValidation = testValidation

testPlayerDoubleValidation :: Trace -> Tick -> IntentCtx 'IPlayerDouble -> Either String () -> Expectation
testPlayerDoubleValidation = testValidation

spec :: Spec
spec = describe "Intent Validation" $ do
    describe "PlayerHit validation" $ do
        it "validates when legal" $ do
            let trace = mkValidationTestTrace testTick
                ctx = PlayerHitCtx testContestantId testHandId
            testValidation trace testTick ctx (Right ())

        it "handles missing entities gracefully" $ do
            let trace = emptyTrace
                ctx = PlayerHitCtx (ContestantId 999) (HandId 888)
            testValidation trace testTick ctx (Left "Validation errors: [EntityNotFound \"HandId 888\",EntityNotFound \"ContestantId 999\"]")

        it "validates regardless of hand state (current implementation)" $ do
            let trace = mkBlackjackTestTrace testTick
                ctx = PlayerHitCtx testContestantId testHandId
            testValidation trace testTick ctx (Right ())

    describe "PlayerStand validation" $ do
        it "validates when legal" $ do
            let trace = mkValidationTestTrace testTick
                ctx = PlayerStandCtx testContestantId testHandId
            testValidation trace testTick ctx (Right ())

        it "handles missing entities gracefully" $ do
            let trace = emptyTrace
                ctx = PlayerStandCtx (ContestantId 999) (HandId 888)
            testValidation trace testTick ctx (Left "Validation errors: [EntityNotFound \"HandId 888\",EntityNotFound \"ContestantId 999\"]")

    describe "PlayerDouble validation" $ do
        it "validates when legal" $ do
            let trace = mkValidationTestTrace testTick
                ctx = PlayerDoubleCtx testContestantId testHandId testPlayerId
            testValidation trace testTick ctx (Right ())

        it "handles missing entities gracefully" $ do
            let trace = emptyTrace
                ctx = PlayerDoubleCtx (ContestantId 999) (HandId 888) (PlayerId 777)
            testValidation trace testTick ctx (Left "Validation errors: [EntityNotFound \"HandId 888\",EntityNotFound \"ContestantId 999\",EntityNotFound \"PlayerId 777\"]")

    describe "PlayerSplit validation" $ do
        it "validates split of pair when legal" $ do
            let pairHand =
                    mkTestHandWithFSM
                        (ContestantOwner testContestantId)
                        (characterize [Card Eight Hearts, Card Eight Spades])
                        (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM))

                contestant = mkTestContestant testPlayerId testRoundId
                player = mkTestPlayer "Test Player"

                trace0 = emptyTrace
                trace1 = applyTraceOp (createBirth testHandId pairHand) testTick trace0
                trace2 = applyTraceOp (createBirth testContestantId contestant) testTick trace1
                trace3 = applyTraceOp (createBirth testPlayerId player) testTick trace2

                ctx = PlayerSplitCtx testContestantId testHandId testPlayerId
            testValidation trace3 testTick ctx (Right ())

        it "rejects split of non-pair" $ do
            let trace = mkValidationTestTrace testTick
                ctx = PlayerSplitCtx testContestantId testHandId testPlayerId
            testValidation trace testTick ctx (Left "Validation errors: [EntityNotFound \"Cannot split: hand is not a pair\"]")

        it "handles missing entities gracefully" $ do
            let trace = emptyTrace
                ctx = PlayerSplitCtx (ContestantId 999) (HandId 888) (PlayerId 777)
            testValidation trace testTick ctx (Left "Validation errors: [EntityNotFound \"HandId 888\",EntityNotFound \"ContestantId 999\",EntityNotFound \"PlayerId 777\"]")

    describe "Validation infrastructure integration" $ do
        it "can access entities through validation context" $ do
            let trace = mkValidationTestTrace testTick
                cache = mkCacheFromTrace trace testTick

                result = withTickCache cache $ do
                    handResult <- deref testHandId
                    contestantResult <- deref testContestantId
                    pure (handResult, contestantResult)

            case result of
                (Just hand, Just contestant) -> do
                    handScore (_hAttrsHand (_hAttrs hand)) `shouldBe` 16
                    _cAttrsActiveHandIx (_cAttrs contestant) `shouldBe` Hand1
                (_, _) -> expectationFailure "Expected to find both hand and contestant"
