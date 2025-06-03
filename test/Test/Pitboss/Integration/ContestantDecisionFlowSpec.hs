{-# LANGUAGE DataKinds #-}

module Test.Pitboss.Integration.ContestantDecisionFlowSpec where

import Control.Lens ((&))
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import System.Random (mkStdGen)
import Test.Hspec
import Test.Pitboss.TestUtils hiding (mkTestHand)

mkTestTable :: Offering -> EntityState 'Table
mkTestTable offering =
    ETable
        (TableAttrs "Test Table" offering (emptyFiniteMap Absent))
        (TableModes (SomeTableFSM TRoundInProgressFSM))
        (TableRels Absent (Present testRoundId))

mkTestHand :: HandOwner -> [Card] -> EntityState 'Hand
mkTestHand owner cards =
    EHand
        (HandAttrs (characterize cards))
        (HandModes (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM)))
        (HandRels owner testBoutId (ShoeId 900))

mkTestTrace :: [Card] -> [Card] -> Trace
mkTestTrace contestantCards dealerCards =
    let contestantHand = mkTestHand (ContestantOwner testContestantId) contestantCards
        dealerHand = mkTestHand (DealerOwner testDealerId) dealerCards
        contestant = mkTestContestant testPlayerId testRoundId
        bout = mkTestBout testHandId testDealerHandId testRoundId (ShoeId 900) testTableId
        table = mkTestTable vegas6
     in emptyTrace
            & applyTraceOp (createBirth testHandId contestantHand) testTick
            & applyTraceOp (createBirth testDealerHandId dealerHand) testTick
            & applyTraceOp (createBirth testContestantId contestant) testTick
            & applyTraceOp (createBirth testBoutId bout) testTick
            & applyTraceOp (createBirth testTableId table) testTick

testIntentGeneration :: String -> [Card] -> BlackjackEvent -> Int -> IO ()
testIntentGeneration description contestantCards expectedEvent seed = do
    let trace = mkInitialTrace testTick
        cardEvents = zipWith CardDealt contestantCards [testHandId, testHandId]
        state0 = SimState trace (EventLog mempty) (IntentLog mempty) testTick
        finalState = foldl (flip runEvent) state0 cardEvents
        cache = mkCacheFromTrace (simTrace finalState) (simTick finalState)

    playerArchetype <- mkTestBasicStrategy
    let gen = mkStdGen seed
        result =
            withTickCache cache $
                generateContestantIntent playerArchetype testContestantId gen

    case result of
        Right event | event == expectedEvent -> pure ()
        Right other -> expectationFailure $ description ++ ": expected " ++ show expectedEvent ++ " but got " ++ show other
        Left _ -> expectationFailure $ description ++ ": expected " ++ show expectedEvent ++ " but got error"

spec :: Spec
spec = describe "Contestant Decision Flow" $ do
    describe "Intent Generation" $ do
        it "generates Surrender for 16 vs 10" $ do
            let trace = mkTestTrace [Card Ten Hearts, Card Six Spades] [Card Ten Diamonds]
                cache = mkCacheFromTrace trace testTick

            playerArchetype <- mkTestBasicStrategy
            let gen = mkStdGen 42
                result =
                    withTickCache cache $
                        generateContestantIntent playerArchetype testContestantId gen

            case result of
                Right (ContestantSurrendered cid hid) -> do
                    cid `shouldBe` testContestantId
                    hid `shouldBe` testHandId
                Left _ -> expectationFailure "Expected ContestantSurrendered but got error"
                Right other -> expectationFailure $ "Expected ContestantSurrendered but got: " ++ show other

        it "generates correct basic strategy decisions" $ do
            testIntentGeneration
                "Hit low hand"
                [Card Five Hearts, Card Seven Spades]
                (ContestantHit testContestantId testHandId)
                43
            testIntentGeneration
                "Stand high hand"
                [Card King Hearts, Card Nine Spades]
                (ContestantStood testContestantId testHandId)
                44
            testIntentGeneration
                "Double when appropriate"
                [Card Six Hearts, Card Five Spades]
                (ContestantDoubledDown testContestantId testHandId)
                45
