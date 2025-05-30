{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pitboss.Integration.BoutFlowSpec (spec) where

import Control.Monad.Reader
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Word (Word64)
import Pitboss.Agency.Archetype.Types
import Pitboss.Blackjack.Events
import Pitboss.Blackjack.Materia.Card
import Pitboss.Blackjack.Materia.Chips
import Pitboss.Blackjack.Materia.Hand
import Pitboss.FSM.Bout
import Pitboss.FSM.PlayerHand
import Pitboss.Sim.Engine.DeltaGen
import Pitboss.Sim.Types
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Sim
import Pitboss.State.TickCache
import Pitboss.State.Trace
import Pitboss.State.Trace.Ops
import Pitboss.State.Types.Core
import Test.Hspec

-- Helper to extract Word64 from Tick
unTick :: Tick -> Word64
unTick (Tick w) = w

mkInitialTrace :: Tick -> Trace
mkInitialTrace startTick =
    let playerId = EntityId 100
        dealerId = EntityId 200
        boutId = EntityId 300
        playerHandId = EntityId 400
        dealerHandId = EntityId 500

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

        dealerState =
            EDealer
                { _dAttrs =
                    DealerAttrs
                        { _dAttrsName = "Test Dealer"
                        , _dAttrsArchetype =
                            SomeDealerByTheBook $
                                ByTheBookDealerArchetype
                                    { btbConfig = ByTheBookConfig (PenetrationProfile 0.75 0.05) (PaceProfile 100 10)
                                    , btbState = ByTheBookState 0
                                    }
                        }
                , _dModes = DealerModes undefined undefined undefined
                , _dRels = DealerRels Nothing Nothing Nothing
                }

        boutState =
            EBout
                { _boutAttrs = BoutAttrs Nothing
                , _boutModes = BoutModes (SomeBoutFSM AwaitingFirstCardFSM)
                , _boutRels = BoutRels playerHandId dealerHandId undefined undefined undefined
                }

        playerHandState =
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

        dealerHandState =
            EDealerHand
                { _dhAttrs = DealerHandAttrs (characterize [])
                , _dhModes = DealerHandModes undefined
                , _dhRels = DealerHandRels undefined dealerId
                }

        trace0 = emptyTrace
        trace1 = applyTraceOp (createBirth playerId playerState) startTick trace0
        trace2 = applyTraceOp (createBirth dealerId dealerState) startTick trace1
        trace3 = applyTraceOp (createBirth boutId boutState) startTick trace2
        trace4 = applyTraceOp (createBirth playerHandId playerHandState) startTick trace3
        trace5 = applyTraceOp (createBirth dealerHandId dealerHandState) startTick trace4
     in trace5

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

runEvent :: BlackjackEvent -> SimState -> SimState
runEvent event state =
    let tick = simTick state
        Tick tickNum = tick
        nextTick = Tick (tickNum + 1)

        -- Fix: Use unTick to extract Word64 from Tick for eventId
        simEvent =
            SimEvent
                { eventId = unTick tick
                , eventOccurred = event
                , eventTimestamp = tick
                , eventCausingIntent = Nothing
                }

        -- Fix: Create TickCacheContext properly
        cache =
            populateTickCache
                (_bouts $ simTrace state)
                (_players $ simTrace state)
                (_playerHands $ simTrace state)
                (_playerSpots $ simTrace state)
                (_dealers $ simTrace state)
                (_dealerHands $ simTrace state)
                (_dealerRounds $ simTrace state)
                (_offerings $ simTrace state)
                (_tables $ simTrace state)
                (_tableShoes $ simTrace state)
                tick

        -- Fix: Wrap Word64 in EntityId constructor for CausalHistory
        traceOps =
            withTickCache cache $
                generateDeltas event (CausalHistory Nothing (Just $ EntityId $ eventId simEvent))

        newTrace = foldl (flip $ \op' -> applyTraceOp op' tick) (simTrace state) traceOps

        -- Fix: Use Tick as key (consistent with EventLog definition)
        newEventLog =
            EventLog $
                IHM.insertWith
                    (++)
                    tick -- Use tick directly, not tickNum
                    [simEvent]
                    (eventLogEvents $ simEventLog state)
     in state
            { simTrace = newTrace
            , simEventLog = newEventLog
            , simTick = nextTick
            }

withSimCache :: SimState -> Reader TickCacheContext a -> a
withSimCache state computation =
    let cache =
            populateTickCache
                (_bouts $ simTrace state)
                (_players $ simTrace state)
                (_playerHands $ simTrace state)
                (_playerSpots $ simTrace state)
                (_dealers $ simTrace state)
                (_dealerHands $ simTrace state)
                (_dealerRounds $ simTrace state)
                (_offerings $ simTrace state)
                (_tables $ simTrace state)
                (_tableShoes $ simTrace state)
                (simTick state)
     in withTickCache cache computation

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
                _phFsm (_phModes hand) `shouldBe` SomePlayerHandFSM (ResolvedFSM Stand)
            Nothing ->
                expectationFailure "Player hand not found"

    it "processes card dealing" $ do
        let state0 = mkInitialSimState
            playerHandId = EntityId 400 :: EntityId 'PlayerHand
            card = Card King Hearts

        let state1 = runEvent (CardDealt card (ToPlayerHand playerHandId)) state0

        let EventLog eventMap = simEventLog state1
            tick = simTick state0
            -- Fix: Use Tick directly for lookup (consistent with EventLog using Tick keys)
            events = IHM.lookup tick eventMap

        case events of
            Just [event] -> eventOccurred event `shouldBe` CardDealt card (ToPlayerHand playerHandId)
            _ -> expectationFailure "Event not logged correctly"
