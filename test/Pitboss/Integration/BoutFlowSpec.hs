{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pitboss.Integration.BoutFlowSpec where

import Control.Monad.Reader
import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Blackjack
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Pitboss.TestHelpers
import Test.Hspec

mkInitialTrace :: Tick -> Trace
mkInitialTrace startTick =
    let playerId = EntityId 100
        dealerId = EntityId 200
        boutId = EntityId 300
        playerHandId = EntityId 400
        dealerHandId = EntityId 500
        playerSpotId = EntityId 600
        dealerRoundId = EntityId 700
        tableId = EntityId 800
        shoeId = EntityId 900

        playerState =
            EPlayer
                { _pAttrs =
                    PlayerAttrs
                        { _pAttrsName = "Test Player"
                        , _pAttrsBankroll = Chips 1000
                        }
                , _pModes =
                    PlayerModes
                        (SomePlayerTableFSM PTPlayingHandFSM)
                        (SomePlayerSpotFSM PSWaitingForHandsFSM)
                        (SomePlayerHandFSM PHDecisionFSM)
                , _pRels = PlayerRels
                }

        dealerState =
            EDealer
                { _dAttrs =
                    DealerAttrs
                        { _dAttrsName = "Test Dealer"
                        }
                , _dModes =
                    DealerModes
                        (SomeDealerTableFSM DTOnDutyFSM)
                        (PeekDealerRound (SomePeekFSM PeekPlayersFSM))
                        (SomeDealerHandFSM DHDealingFSM)
                , _dRels = DealerRels (Just tableId) (Just dealerRoundId) (Just dealerHandId)
                }

        boutState =
            EBout
                { _boutAttrs = BoutAttrs Nothing
                , _boutModes = BoutModes (SomeBoutFSM BAwaitingFirstCardFSM)
                , _boutRels = BoutRels playerHandId dealerHandId shoeId tableId dealerRoundId
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
                , _phModes = PlayerHandModes (SomePlayerHandFSM PHDecisionFSM)
                , _phRels = PlayerHandRels playerSpotId dealerRoundId playerId boutId
                }

        dealerHandState =
            EDealerHand
                { _dhAttrs = DealerHandAttrs (characterize [])
                , _dhModes = DealerHandModes (SomeDealerHandFSM DHDealingFSM)
                , _dhRels = DealerHandRels dealerRoundId dealerId
                }

        playerSpotState =
            EPlayerSpot
                { _psAttrs = PlayerSpotAttrs EPlayerSpot1 (Chips 100)
                , _psModes = PlayerSpotModes (SomePlayerSpotFSM PSWaitingForHandsFSM)
                , _psRels = PlayerSpotRels playerId dealerRoundId (emptyFiniteMap Absent)
                }

        dealerRoundState =
            EDealerRound
                { _drAttrs = DealerRoundAttrs 1 True
                , _drModes = DealerRoundModes
                , _drRels = DealerRoundRels shoeId
                }

        tableState =
            ETable
                { _tAttrs =
                    TableAttrs
                        { _tAttrsName = "Test Table"
                        , _tAttrsCurrentRound = Just dealerRoundId
                        , _tAttrsOffering = vegas6
                        }
                , _tModes = TableModes (SomeTableFSM TRoundInProgressFSM)
                , _tRels = TableRels (Just dealerId)
                }

        shoeState =
            ETableShoe
                { _tsAttrs =
                    TableShoeAttrs
                        [ Card Ten Hearts
                        , Card Six Diamonds
                        , Card Seven Spades
                        , Card King Clubs
                        , Card Four Hearts
                        ]
                        mempty
                , _tsModes = TableShoeModes
                , _tsRels = TableShoeRels tableId
                }

        trace0 = emptyTrace
        trace1 = applyTraceOp (createBirth playerId playerState) startTick trace0
        trace2 = applyTraceOp (createBirth dealerId dealerState) startTick trace1
        trace3 = applyTraceOp (createBirth boutId boutState) startTick trace2
        trace4 = applyTraceOp (createBirth playerHandId playerHandState) startTick trace3
        trace5 = applyTraceOp (createBirth dealerHandId dealerHandState) startTick trace4
        trace6 = applyTraceOp (createBirth playerSpotId playerSpotState) startTick trace5
        trace7 = applyTraceOp (createBirth dealerRoundId dealerRoundState) startTick trace6
        trace8 = applyTraceOp (createBirth tableId tableState) startTick trace7
        trace9 = applyTraceOp (createBirth shoeId shoeState) startTick trace8
     in trace9

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

        simEvent =
            SimEvent
                { eventId = unTick tick
                , eventOccurred = event
                , eventTimestamp = tick
                , eventCausingIntent = Nothing
                }

        cache =
            populateTickCache
                (_bouts $ simTrace state)
                (_players $ simTrace state)
                (_playerHands $ simTrace state)
                (_playerSpots $ simTrace state)
                (_dealers $ simTrace state)
                (_dealerHands $ simTrace state)
                (_dealerRounds $ simTrace state)
                (_tables $ simTrace state)
                (_tableShoes $ simTrace state)
                tick

        traceOps =
            withTickCache cache $
                generateDeltas event (CausalHistory Nothing (Just $ EntityId $ eventId simEvent))

        newTrace = foldl (flip $ \op' -> applyTraceOp op' tick) (simTrace state) traceOps

        newEventLog =
            EventLog $
                IHM.insertWith
                    (++)
                    tick
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
            dealerId = EntityId 200 :: EntityId 'Dealer
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
            playerId = EntityId 100 :: EntityId 'Player
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
