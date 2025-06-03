{-# LANGUAGE DataKinds #-}

module Test.Pitboss.Unit.Simulation.IntentValidationSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.TestUtils

mkValidationTestTrace :: Tick -> Trace
mkValidationTestTrace startTick =
    let
        -- Create single unified Bout entity with player hand
        boutPlayer = mkTestBout testPlayerId testDealerId testRoundId testTableId
        boutPlayerWithHand = boutPlayer{_bAttrs = (_bAttrs boutPlayer){_bAttrsPlayerHand = characterize [Card Ten Hearts, Card Six Spades]}}

        -- Create separate busted bout for different tests
        bustedBoutPlayer = mkTestBout testPlayerId testDealerId testRoundId testTableId
        bustedBoutPlayerWithHand = bustedBoutPlayer{_bAttrs = (_bAttrs bustedBoutPlayer){_bAttrsPlayerHand = characterize [Card Ten Hearts, Card Six Spades, Card King Clubs]}}

        player = mkTestPlayer "Test Player"
        table =
            ETable
                (TableAttrs "Test Table" vegas6 (emptyFiniteMap Absent))
                (TableModes (SomeTableFSM TRoundInProgressFSM))
                (TableRels Absent (Present testRoundId))

        roundState =
            ERound
                (RoundAttrs Absent 1)
                RoundModes
                (RoundRels (singletonFiniteMap TableSpot1 (Present testBoutId) Absent) testShoeId testTableId)

        trace0 = emptyTrace
        trace1 = applyTraceOp (bear testBoutId boutPlayerWithHand) startTick trace0
        trace2 = applyTraceOp (bear (EntityId 999 :: BoutId) bustedBoutPlayerWithHand) startTick trace1
        trace3 = applyTraceOp (bear testPlayerId player) startTick trace2
        trace4 = applyTraceOp (bear testTableId table) startTick trace3
        trace5 = applyTraceOp (bear testRoundId roundState) startTick trace4
     in
        trace5

mkBlackjackTestTrace :: Tick -> Trace
mkBlackjackTestTrace startTick =
    let boutPlayer = mkTestBout testPlayerId testDealerId testRoundId testTableId
        boutPlayerWithBlackjack = boutPlayer{_bAttrs = (_bAttrs boutPlayer){_bAttrsPlayerHand = characterize [Card Ace Hearts, Card King Spades]}}

        player = mkTestPlayer "Test Player"
        table =
            ETable
                (TableAttrs "Test Table" vegas6 (emptyFiniteMap Absent))
                (TableModes (SomeTableFSM TRoundInProgressFSM))
                (TableRels Absent (Present testRoundId))

        roundState =
            ERound
                (RoundAttrs Absent 1)
                RoundModes
                (RoundRels (singletonFiniteMap TableSpot1 (Present testBoutId) Absent) testShoeId testTableId)

        trace0 = emptyTrace
        trace1 = applyTraceOp (bear testBoutId boutPlayerWithBlackjack) startTick trace0
        trace2 = applyTraceOp (bear testPlayerId player) startTick trace1
        trace3 = applyTraceOp (bear testTableId table) startTick trace2
        trace4 = applyTraceOp (bear testRoundId roundState) startTick trace3
     in trace4

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

testBoutPlayerHitValidation :: Trace -> Tick -> IntentCtx 'IBoutPlayerHit -> Either String () -> Expectation
testBoutPlayerHitValidation = testValidation

testBoutPlayerStandValidation :: Trace -> Tick -> IntentCtx 'IBoutPlayerStand -> Either String () -> Expectation
testBoutPlayerStandValidation = testValidation

testBoutPlayerDoubleValidation :: Trace -> Tick -> IntentCtx 'IBoutPlayerDouble -> Either String () -> Expectation
testBoutPlayerDoubleValidation = testValidation

spec :: Spec
spec = describe "Intent Validation" $ do
    describe "BoutPlayerHit validation" $ do
        it "validates when legal" $ do
            let trace = mkValidationTestTrace testTick
                ctx = BoutPlayerHitCtx testBoutId
            testValidation trace testTick ctx (Right ())

        it "handles missing entities gracefully" $ do
            let trace = emptyTrace
                ctx = BoutPlayerHitCtx (EntityId 999 :: BoutId)
            testValidation trace testTick ctx (Left "Validation errors: [EntityNotFound \"EntityId 999\"]")

        it "validates regardless of hand state (current implementation)" $ do
            let trace = mkBlackjackTestTrace testTick
                ctx = BoutPlayerHitCtx testBoutId
            testValidation trace testTick ctx (Right ())

    describe "BoutPlayerStand validation" $ do
        it "validates when legal" $ do
            let trace = mkValidationTestTrace testTick
                ctx = BoutPlayerStandCtx testBoutId
            testValidation trace testTick ctx (Right ())

        it "handles missing entities gracefully" $ do
            let trace = emptyTrace
                ctx = BoutPlayerStandCtx (EntityId 999 :: BoutId)
            testValidation trace testTick ctx (Left "Validation errors: [EntityNotFound \"EntityId 999\"]")

    describe "BoutPlayerDouble validation" $ do
        it "validates when legal" $ do
            let trace = mkValidationTestTrace testTick
                ctx = BoutPlayerDoubleCtx testBoutId testPlayerId testTableId
            testValidation trace testTick ctx (Right ())

        it "handles missing entities gracefully" $ do
            let trace = emptyTrace
                ctx = BoutPlayerDoubleCtx (EntityId 999 :: BoutId) (EntityId 777 :: PlayerId) testTableId
            testValidation trace testTick ctx (Left "Validation errors: [EntityNotFound \"EntityId 999\",EntityNotFound \"EntityId 777\"]")

    describe "BoutPlayerSplit validation" $ do
        it "validates split of pair when legal" $ do
            let boutPlayer = mkTestBout testPlayerId testDealerId testRoundId testTableId
                boutPlayerWithPair = boutPlayer{_bAttrs = (_bAttrs boutPlayer){_bAttrsPlayerHand = characterize [Card Eight Hearts, Card Eight Spades]}}

                player = mkTestPlayer "Test Player"
                table =
                    ETable
                        (TableAttrs "Test Table" vegas6 (emptyFiniteMap Absent))
                        (TableModes (SomeTableFSM TRoundInProgressFSM))
                        (TableRels Absent (Present testRoundId))

                roundState =
                    ERound
                        (RoundAttrs Absent 1)
                        RoundModes
                        (RoundRels (singletonFiniteMap TableSpot1 (Present testBoutId) Absent) testShoeId testTableId)

                trace0 = emptyTrace
                trace1 = applyTraceOp (bear testBoutId boutPlayerWithPair) testTick trace0
                trace2 = applyTraceOp (bear testPlayerId player) testTick trace1
                trace3 = applyTraceOp (bear testTableId table) testTick trace2
                trace4 = applyTraceOp (bear testRoundId roundState) testTick trace3

                ctx = BoutPlayerSplitCtx testBoutId testPlayerId testTableId
            testValidation trace4 testTick ctx (Right ())

        it "rejects split of non-pair" $ do
            let trace = mkValidationTestTrace testTick
                ctx = BoutPlayerSplitCtx testBoutId testPlayerId testTableId
            testValidation trace testTick ctx (Left "Validation errors: [EntityNotFound \"Cannot split: hand is not a pair\"]")

        it "handles missing entities gracefully" $ do
            let trace = emptyTrace
                ctx = BoutPlayerSplitCtx (EntityId 999 :: BoutId) (EntityId 777 :: PlayerId) testTableId
            testValidation trace testTick ctx (Left "Validation errors: [EntityNotFound \"EntityId 999\",EntityNotFound \"EntityId 777\"]")

    describe "Validation infrastructure integration" $ do
        it "can access entities through validation context" $ do
            let trace = mkValidationTestTrace testTick
                cache = mkCacheFromTrace trace testTick

                result = withTickCache cache $ do
                    deref testBoutId

            case result of
                Just boutPlayer -> do
                    handScore (_bAttrsPlayerHand (_bAttrs boutPlayer)) `shouldBe` 16
                    _bAttrsActiveHandIx (_bAttrs boutPlayer) `shouldBe` Hand1
                Nothing -> expectationFailure "Expected to find boutPlayer"
