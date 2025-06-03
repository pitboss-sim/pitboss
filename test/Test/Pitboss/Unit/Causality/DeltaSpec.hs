{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Unit.Causality.DeltaSpec where

import Control.Monad.Reader
import Pitboss.Blackjack hiding (Hand, HandWitness)
import Pitboss.Causality
import Pitboss.Causality.Validate (ValidationErrors)
import Pitboss.FSM.Types
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "DeltaGen" $ do
    describe "contestant events" $ do
        it "generates mutation for contestant stand" $ do
            let trace = mkTestTrace testTick
                traceOps = generateDeltasForEvent trace (ContestantStood testContestantId testHandId) testCausalHistory

            case traceOps of
                Right ops -> do
                    ops `shouldHaveLength` 1
                    shouldContainHandModesMutation ops testHandId testCausalHistory
                Left errors -> expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

        it "generates mutation for contestant hit" $ do
            let trace = mkTestTrace testTick
                traceOps = generateDeltasForEvent trace (ContestantHit testContestantId testHandId) testCausalHistory

            case traceOps of
                Right ops -> do
                    ops `shouldHaveLength` 1
                    shouldContainHandModesMutation ops testHandId testCausalHistory
                Left errors -> expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

        it "generates mutation for contestant double down" $ do
            let trace = mkTestTraceForDoubling testTick
                traceOps = generateDeltasForEvent trace (ContestantDoubledDown testContestantId testHandId) testCausalHistory

            case traceOps of
                Right ops -> do
                    ops `shouldHaveLength` 1
                    shouldContainHandModesMutation ops testHandId testCausalHistory
                Left errors -> expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

    describe "card dealing events" $ do
        it "generates hand update for card dealt to contestant hand" $ do
            let trace = mkTestTrace testTick
                traceOps = generateDeltasForEvent trace (CardDealt (Card King Hearts) testHandId) emptyCausalHistory

            case traceOps of
                Right ops -> do
                    length ops `shouldSatisfy` (>= 1)
                    shouldContainHandAttrsMutation ops testHandId
                Left errors -> expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

        it "handles dealer hand card dealing" $ do
            let trace = mkTestTrace testTick
                traceOps = generateDeltasForEvent trace (CardDealt (Card Ace Spades) testDealerHandId) emptyCausalHistory

            case traceOps of
                Right ops -> do
                    length ops `shouldSatisfy` (>= 1)
                    shouldContainHandAttrsMutation ops testDealerHandId
                Left errors -> expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

        it "generates FSM mutations when card affects hand state" $ do
            let trace = mkTestTraceWithAwaitingHand testTick
                traceOps = generateDeltasForEvent trace (CardDealt (Card King Hearts) testHandId) emptyCausalHistory

            case traceOps of
                Right ops -> do
                    length ops `shouldSatisfy` (>= 2)
                    shouldContainHandAttrsMutation ops testHandId
                    shouldContainHandModesMutation ops testHandId emptyCausalHistory
                Left errors -> expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

    describe "dealer events" $ do
        it "generates mutation for dealer revealed" $ do
            let trace = mkTestTrace testTick
                traceOps = generateDeltasForEvent trace (DealerRevealed testDealerId testDealerHandId) testCausalHistory

            case traceOps of
                Right ops -> do
                    ops `shouldHaveLength` 1
                    shouldContainHandModesMutation ops testDealerHandId testCausalHistory
                Left errors -> expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

        it "generates mutation for dealer stood" $ do
            let trace = mkTestTrace testTick
                traceOps = generateDeltasForEvent trace (DealerStood testDealerId testDealerHandId) testCausalHistory

            case traceOps of
                Right ops -> do
                    ops `shouldHaveLength` 1
                    shouldContainHandModesMutation ops testDealerHandId testCausalHistory
                Left errors -> expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

    describe "bout events" $ do
        it "generates bout attrs mutation for bout settled" $ do
            let trace = mkTestTrace testTick
                traceOps = generateDeltasForEvent trace (BoutSettled testBoutId dealerWinsHigher) testCausalHistory

            case traceOps of
                Right ops -> do
                    ops `shouldHaveLength` 1
                    shouldContainBoutAttrsMutation ops testBoutId testCausalHistory
                Left errors -> expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

    describe "error handling" $ do
        it "returns validation errors when entities are missing" $ do
            let trace = emptyTrace
                traceOps = generateDeltasForEvent trace (ContestantStood (ContestantId 999) (HandId 888)) testCausalHistory

            case traceOps of
                Right _ops -> expectationFailure "Expected validation errors for missing entities"
                Left _errors -> pure ()

-- Helper Functions
generateDeltasForEvent :: Trace -> BlackjackEvent -> CausalHistory -> Either ValidationErrors [TraceOp]
generateDeltasForEvent trace event causalHistory =
    let cache = mkCacheFromTrace trace testTick
        context' = TickCacheContext cache testTick
     in runReader (generateDeltas event causalHistory) context'

mkTestTrace :: Tick -> Trace
mkTestTrace startTick =
    let playerState = mkTestPlayer "Blackjack Player"
        handState = mkTestContestantHand testContestantId
        contestantState = mkTestContestant testPlayerId testRoundId
        dealerState = mkTestDealer "Test Dealer"
        dealerHandState = mkTestDealerHand testDealerId
        boutState = mkTestBout testHandId testDealerHandId testRoundId testShoeId testTableId

        trace0 = emptyTrace
        trace1 = applyTraceOp (createBirth testPlayerId playerState) startTick trace0
        trace2 = applyTraceOp (createBirth testHandId handState) startTick trace1
        trace3 = applyTraceOp (createBirth testContestantId contestantState) startTick trace2
        trace4 = applyTraceOp (createBirth testDealerId dealerState) startTick trace3
        trace5 = applyTraceOp (createBirth testDealerHandId dealerHandState) startTick trace4
        trace6 = applyTraceOp (createBirth testBoutId boutState) startTick trace5
     in trace6

mkTestTraceForDoubling :: Tick -> Trace
mkTestTraceForDoubling startTick =
    let playerState = mkTestPlayer "Blackjack Player"
        handState = mkTestContestantHandForDoubling testContestantId
        contestantState = mkTestContestant testPlayerId testRoundId
        dealerState = mkTestDealer "Test Dealer"
        dealerHandState = mkTestDealerHand testDealerId
        boutState = mkTestBout testHandId testDealerHandId testRoundId testShoeId testTableId

        trace0 = emptyTrace
        trace1 = applyTraceOp (createBirth testPlayerId playerState) startTick trace0
        trace2 = applyTraceOp (createBirth testHandId handState) startTick trace1
        trace3 = applyTraceOp (createBirth testContestantId contestantState) startTick trace2
        trace4 = applyTraceOp (createBirth testDealerId dealerState) startTick trace3
        trace5 = applyTraceOp (createBirth testDealerHandId dealerHandState) startTick trace4
        trace6 = applyTraceOp (createBirth testBoutId boutState) startTick trace5
     in trace6

mkTestContestantHandForDoubling :: ContestantId -> EntityState 'Hand
mkTestContestantHandForDoubling contestantId =
    let validDoublingHand = characterize [Card Five Hearts, Card Six Spades]
        handAttrs = HandAttrs validDoublingHand
        handModes = HandModes (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM))
        handRels = HandRels (ContestantOwner contestantId) testBoutId testShoeId
     in EHand handAttrs handModes handRels

mkTestTraceWithAwaitingHand :: Tick -> Trace
mkTestTraceWithAwaitingHand startTick =
    let playerState = mkTestPlayer "Blackjack Player"
        handState = mkTestContestantHandAwaitingFirst testContestantId
        contestantState = mkTestContestant testPlayerId testRoundId
        dealerState = mkTestDealer "Test Dealer"
        dealerHandState = mkTestDealerHand testDealerId
        boutState = mkTestBout testHandId testDealerHandId testRoundId testShoeId testTableId

        trace0 = emptyTrace
        trace1 = applyTraceOp (createBirth testPlayerId playerState) startTick trace0
        trace2 = applyTraceOp (createBirth testHandId handState) startTick trace1
        trace3 = applyTraceOp (createBirth testContestantId contestantState) startTick trace2
        trace4 = applyTraceOp (createBirth testDealerId dealerState) startTick trace3
        trace5 = applyTraceOp (createBirth testDealerHandId dealerHandState) startTick trace4
        trace6 = applyTraceOp (createBirth testBoutId boutState) startTick trace5
     in trace6

-- Test Assertion Helpers
shouldHaveLength :: [a] -> Int -> Expectation
shouldHaveLength list expected = length list `shouldBe` expected

shouldContainHandModesMutation :: [TraceOp] -> HandId -> CausalHistory -> Expectation
shouldContainHandModesMutation traceOps expectedId expectedHistory =
    case filter isHandModesMutation traceOps of
        (MutationOp HandWitness entityId (ModesDelta actualHistory _) : _) -> do
            entityId `shouldBe` expectedId
            causalIntent actualHistory `shouldBe` causalIntent expectedHistory
            causalEvent actualHistory `shouldBe` causalEvent expectedHistory
        [] -> expectationFailure $ "Expected hand modes mutation for " ++ show expectedId ++ " but got: " ++ show traceOps
        _ -> expectationFailure $ "Found unexpected mutations: " ++ show traceOps
  where
    isHandModesMutation (MutationOp HandWitness entityId (ModesDelta _ _)) = entityId == expectedId
    isHandModesMutation _ = False

shouldContainHandAttrsMutation :: [TraceOp] -> HandId -> Expectation
shouldContainHandAttrsMutation traceOps expectedHandId =
    case filter isHandAttrsMutation traceOps of
        [] -> expectationFailure $ "Expected hand attrs mutation for " ++ show expectedHandId
        _ -> pure ()
  where
    isHandAttrsMutation (MutationOp HandWitness entityId (AttrsDelta _ _)) = entityId == expectedHandId
    isHandAttrsMutation _ = False

shouldContainBoutAttrsMutation :: [TraceOp] -> BoutId -> CausalHistory -> Expectation
shouldContainBoutAttrsMutation traceOps expectedId expectedHistory =
    case filter isBoutAttrsMutation traceOps of
        (MutationOp BoutWitness entityId (AttrsDelta actualHistory _) : _) -> do
            entityId `shouldBe` expectedId
            causalIntent actualHistory `shouldBe` causalIntent expectedHistory
            causalEvent actualHistory `shouldBe` causalEvent expectedHistory
        [] -> expectationFailure $ "Expected bout attrs mutation for " ++ show expectedId ++ " but got: " ++ show traceOps
        _ -> expectationFailure $ "Found unexpected mutations: " ++ show traceOps
  where
    isBoutAttrsMutation (MutationOp BoutWitness entityId (AttrsDelta _ _)) = entityId == expectedId
    isBoutAttrsMutation _ = False
