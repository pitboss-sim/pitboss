{-# LANGUAGE DataKinds #-}

module Spec.Pitboss.Orchestration.DeltaGenSpec where

import Control.Monad.Reader
import Pitboss.Causality
import Pitboss.Simulation
import Spec.Pitboss.Helpers
import Test.Hspec

mkTestTrace :: Tick -> IO Trace
mkTestTrace startTick = do
    let playerId = EntityId 100
    let handId = EntityId 400
    let spotId = EntityId 500
    let boutId = EntityId 300
    let roundId = EntityId 600

    playerState <- mkTestPlayer playerId "Test Player"

    let handState = mkTestPlayerHand handId spotId roundId playerId boutId
    let trace0 = emptyTrace
    let trace1 = applyTraceOp (createBirth playerId playerState) startTick trace0
    let trace2 = applyTraceOp (createBirth handId handState) startTick trace1

    return trace2

mkCacheFromTrace :: Trace -> Tick -> TickCache
mkCacheFromTrace trace =
    populateTickCache
        (_bouts trace)
        (_players trace)
        (_playerHands trace)
        (_playerSpots trace)
        (_dealers trace)
        (_dealerHands trace)
        (_dealerRounds trace)
        (_tables trace)
        (_tableShoes trace)

spec :: Spec
spec = describe "DeltaGen" $ do
    it "generates trace operations for player stand event" $ do
        trace <- mkTestTrace (Tick 1000)
        let tick = Tick 1000
            playerId = EntityId 100
            handId = EntityId 400
            event = PlayerStood playerId handId

            causalHistory =
                CausalHistory
                    { causalIntent = Just (EntityId 123)
                    , causalEvent = Just (EntityId 456)
                    }

            cache = mkCacheFromTrace trace tick
            context' = TickCacheContext cache tick

            traceOps = runReader (generateDeltas event causalHistory) context'

        length traceOps `shouldBe` 1

        case traceOps of
            [traceOp] -> case traceOp of
                MutationOp witness' eid someDelta -> do
                    show witness' `shouldBe` show PlayerHandWitness
                    eid `shouldBe` handId
                    case someDelta of
                        ModesDelta hist _delta -> do
                            causalIntent hist `shouldBe` Just (EntityId 123)
                            causalEvent hist `shouldBe` Just (EntityId 456)
                            pure ()
                        _ -> expectationFailure "Expected modes delta"
                _ -> expectationFailure "Expected mutation operation"
            [] -> expectationFailure "Expected one mutation operation but got none"
            _ -> expectationFailure "Expected exactly one mutation operation"

    it "generates empty list when hand not found" $ do
        let tick = Tick 1000
            event = PlayerStood (EntityId 999) (EntityId 888)
            causalHistory = CausalHistory Nothing Nothing

            trace = emptyTrace
            cache = mkCacheFromTrace trace tick
            context' = TickCacheContext cache tick

            traceOps = runReader (generateDeltas event causalHistory) context'

        case traceOps of
            [] -> pure ()
            _ -> expectationFailure "Expected empty list when entities don't exist"
