{-# LANGUAGE DataKinds #-}

module Pitboss.Orchestration.DeltaGenSpec (spec) where

import Control.Monad.Reader
import Pitboss.Agency.Archetype.Types
import Pitboss.Blackjack.Events
import Pitboss.Blackjack.Materia.Chips
import Pitboss.Blackjack.Materia.Hand
import Pitboss.FSM.PlayerHand
import Pitboss.Sim.Engine.DeltaGen
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.TickCache
import Pitboss.State.Trace
import Pitboss.State.Trace.Ops
import Pitboss.State.Types.Core
import Test.Hspec

-- Helper to create a trace with test entities
mkTestTrace :: Tick -> Trace
mkTestTrace startTick =
    let playerId = EntityId 100
        handId = EntityId 400

        playerState =
            EPlayer
                { _pAttrs =
                    PlayerAttrs
                        { _pAttrsName = "Test Player"
                        , _pAttrsBankroll = Chips 1000
                        , _pAttrsArchetype =
                            SomePlayerBasicStrategy $
                                BasicStrategyArchetype
                                    { bsConfig = BasicConfig undefined (MistakeProfile 0 undefined)
                                    , bsState = BasicState 0 emptySessionStats
                                    }
                        }
                , _pModes = PlayerModes undefined undefined undefined
                , _pRels = PlayerRels
                }

        handState =
            EPlayerHand
                { _phAttrs =
                    PlayerHandAttrs
                        { _phAttrsHand = characterize []
                        , _phAttrsOriginalBet = Chips 100
                        , _phAttrsSplitDepth = 0
                        , _phAttrsHandIx = 0
                        }
                , _phModes = PlayerHandModes (SomePlayerHandFSM DecisionFSM)
                , _phRels = PlayerHandRels undefined undefined playerId
                }

        trace0 = emptyTrace
        trace1 = applyTraceOp (createBirth playerId playerState) startTick trace0
        trace2 = applyTraceOp (createBirth handId handState) startTick trace1
     in trace2

-- Helper to create cache from trace
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
        (_offerings trace)
        (_tables trace)
        (_tableShoes trace)

spec :: Spec
spec = describe "DeltaGen" $ do
    it "generates trace operations for player stand event" $ do
        let tick = Tick 1000
            playerId = EntityId 100
            handId = EntityId 400
            event = PlayerStood playerId handId

            causalHistory =
                CausalHistory
                    { causalIntent = Just (EntityId 123)
                    , causalEvent = Just (EntityId 456)
                    }

            trace = mkTestTrace tick
            cache = mkCacheFromTrace trace tick
            context' = TickCacheContext cache tick

            traceOps = runReader (generateDeltas event causalHistory) context'

        -- Should generate one mutation operation
        length traceOps `shouldBe` 1

        -- Fix: Pattern match without constraining the type variable
        case traceOps of
            [traceOp] -> case traceOp of
                MutationOp witness' eid someDelta -> do
                    -- Check witness using show since we can't directly compare GADTs
                    show witness' `shouldBe` show PlayerHandWitness
                    eid `shouldBe` handId
                    case someDelta of
                        ModesDelta hist _delta -> do
                            causalIntent hist `shouldBe` Just (EntityId 123)
                            causalEvent hist `shouldBe` Just (EntityId 456)
                            -- We can't safely pattern match on the specific delta constructor
                            -- due to GADT constraints, but we can verify it's a modes delta
                            pure ()
                        _ -> expectationFailure "Expected modes delta"
                _ -> expectationFailure "Expected mutation operation"
            [] -> expectationFailure "Expected one mutation operation but got none"
            _ -> expectationFailure "Expected exactly one mutation operation"

    it "generates empty list when hand not found" $ do
        let tick = Tick 1000
            event = PlayerStood (EntityId 999) (EntityId 888) -- non-existent IDs
            causalHistory = CausalHistory Nothing Nothing

            trace = emptyTrace -- empty trace, no entities
            cache = mkCacheFromTrace trace tick
            context' = TickCacheContext cache tick

            traceOps = runReader (generateDeltas event causalHistory) context'

        -- Should generate no operations when entities don't exist
        case traceOps of
            [] -> pure () -- success
            _ -> expectationFailure "Expected empty list when entities don't exist"
