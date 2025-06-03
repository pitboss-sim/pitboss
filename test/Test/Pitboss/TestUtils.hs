{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Pitboss.TestUtils where

import Control.Exception (handle)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (Reader)
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Text.IO qualified as TIO
import GHC.Exception (SomeException)
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Pitboss.Simulation.Delta.Generate
import Test.QuickCheck (Arbitrary (..), elements, oneof)

testTick :: Tick
testTick = Tick 1000

testEmptyHand :: SomeHand
testEmptyHand = SomeHand [] (HandWitness HardWitness EmptyWitness NoneWitness [] 0)

testPlayerId :: PlayerId
testPlayerId = EntityId 1200 :: PlayerId

testBoutId :: BoutId
testBoutId = EntityId 100 :: BoutId

testDealerId :: DealerId
testDealerId = EntityId 200 :: DealerId

testRoundId :: RoundId
testRoundId = EntityId 700 :: RoundId

testTableId :: TableId
testTableId = EntityId 800 :: TableId

testShoeId :: ShoeId
testShoeId = EntityId 900 :: ShoeId

testCausalHistory :: CausalHistory
testCausalHistory = CausalHistory (Just (IntentId 123)) (Just (EventId 456))

emptyCausalHistory :: CausalHistory
emptyCausalHistory = CausalHistory Nothing Nothing

hardSixteen :: SomeHand
hardSixteen = characterize [Card Ten Hearts, Card Six Spades]

softSeventeen :: SomeHand
softSeventeen = characterize [Card Ace Hearts, Card Six Spades]

blackjackHand :: SomeHand
blackjackHand = characterize [Card Ace Hearts, Card King Spades]

bustHand :: SomeHand
bustHand = characterize [Card Ten Hearts, Card Six Spades, Card King Clubs]

pairOfEights :: SomeHand
pairOfEights = characterize [Card Eight Hearts, Card Eight Spades]

hardSeventeen :: SomeHand
hardSeventeen = characterize [Card Ten Hearts, Card Seven Spades]

instance Arbitrary Rank where
    arbitrary = elements [Two .. Ace]

instance Arbitrary Suit where
    arbitrary = elements [Hearts .. Spades]

instance Arbitrary Card where
    arbitrary = Card <$> arbitrary <*> arbitrary

instance Arbitrary Chips where
    arbitrary = Chips . abs <$> arbitrary

instance Arbitrary BoutId where
    arbitrary = EntityId . abs <$> arbitrary

instance Arbitrary DealerId where
    arbitrary = EntityId . abs <$> arbitrary

instance Arbitrary RoundId where
    arbitrary = EntityId . abs <$> arbitrary

instance Arbitrary TableId where
    arbitrary = EntityId . abs <$> arbitrary

instance Arbitrary ShoeId where
    arbitrary = EntityId . abs <$> arbitrary

instance Arbitrary Tick where
    arbitrary = Tick . abs <$> arbitrary

instance Arbitrary IntentId where
    arbitrary = IntentId . abs <$> arbitrary

instance Arbitrary EventId where
    arbitrary = EventId . abs <$> arbitrary

instance Arbitrary CausalHistory where
    arbitrary = CausalHistory <$> arbitrary <*> arbitrary

instance Arbitrary TableSpotIx where
    arbitrary = elements [TableSpot1 .. TableSpot6]

instance Arbitrary HandIx where
    arbitrary = elements [Hand1 .. Hand4]

instance Arbitrary DetailedOutcome where
    arbitrary =
        oneof
            [ pure boutPlayerWinsHigher
            , pure boutPlayerWinsDealerBust
            , pure boutPlayerWinsBlackjack
            , pure dealerWinsHigher
            , pure dealerWinsBoutPlayerBust
            , pure dealerWinsBlackjack
            , pure pushOutcome
            ]

mkCacheFromTrace :: Trace -> Tick -> TickCache
mkCacheFromTrace trace =
    populateTickCache
        (_bouts trace)
        (_dealers trace)
        (_players trace)
        (_rounds trace)
        (_shoes trace)
        (_tables trace)

withSimCache :: SimState -> Reader TickCacheContext a -> a
withSimCache state computation =
    let cache = mkCacheFromTrace (simTrace state) (simTick state)
     in withTickCache cache computation

mkTestPlayer :: String -> EntityState 'Player
mkTestPlayer name =
    EPlayer
        (PlayerAttrs name (Chips 1000))
        (PlayerModes (SomePlayerFSM PPlayingHandFSM))
        (PlayerRels Absent Absent)

mkTestDealer :: String -> EntityState 'Dealer
mkTestDealer name =
    EDealer
        (DealerAttrs name Absent)
        (DealerModes (SomeDealerTableFSM DOffDutyFSM) (PeekRound (SomePeekFSM PeekAwaitingFSM)))
        (DealerRels Absent Absent)

mkTestBoutPlayerPlaying :: PlayerId -> DealerId -> RoundId -> TableId -> EntityState 'Bout
mkTestBoutPlayerPlaying playerId dealerId roundId tableId =
    EBout
        (BoutAttrs testEmptyHand testEmptyHand Hand1 Absent)
        (BoutModes (SomeBoutFSM BPlayerTurnFSM) (SomePlayerHandFSM PHDecisionFSM) (SomeDealerHandFSM DHEvaluatingFSM))
        (BoutRels playerId dealerId roundId tableId (singletonFiniteMap Hand1 (Present testBoutId) Absent))

mkTestBoutPlayerAwaitingCards :: PlayerId -> DealerId -> RoundId -> TableId -> EntityState 'Bout
mkTestBoutPlayerAwaitingCards playerId dealerId roundId tableId =
    EBout
        (BoutAttrs testEmptyHand testEmptyHand Hand1 Absent)
        (BoutModes (SomeBoutFSM BAwaitingFirstCardFSM) (SomePlayerHandFSM PHAwaitingFirstCardFSM) (SomeDealerHandFSM DHAwaitingFirstCardFSM))
        (BoutRels playerId dealerId roundId tableId (singletonFiniteMap Hand1 (Present testBoutId) Absent))

mkTestBoutPlayerResolved :: PlayerHandResolution -> PlayerId -> DealerId -> RoundId -> TableId -> EntityState 'Bout
mkTestBoutPlayerResolved resolution playerId dealerId roundId tableId =
    EBout
        (BoutAttrs testEmptyHand testEmptyHand Hand1 Absent)
        (BoutModes (SomeBoutFSM BDoneFSM) (SomePlayerHandFSM (PHResolvedFSM resolution)) (SomeDealerHandFSM DHEvaluatingFSM))
        (BoutRels playerId dealerId roundId tableId (singletonFiniteMap Hand1 (Present testBoutId) Absent))

mkTestBout :: PlayerId -> DealerId -> RoundId -> TableId -> EntityState 'Bout
mkTestBout playerId dealerId roundId tableId =
    EBout
        (BoutAttrs testEmptyHand testEmptyHand Hand1 Absent)
        (BoutModes (SomeBoutFSM BPlayerTurnFSM) (SomePlayerHandFSM PHDecisionFSM) (SomeDealerHandFSM DHEvaluatingFSM))
        (BoutRels playerId dealerId roundId tableId (singletonFiniteMap Hand1 (Present testBoutId) Absent))

mkTestBoutDealerAwaitingCards :: PlayerId -> DealerId -> RoundId -> TableId -> EntityState 'Bout
mkTestBoutDealerAwaitingCards playerId dealerId roundId tableId =
    EBout
        (BoutAttrs testEmptyHand testEmptyHand Hand1 Absent)
        (BoutModes (SomeBoutFSM BAwaitingFirstCardFSM) (SomePlayerHandFSM PHAwaitingFirstCardFSM) (SomeDealerHandFSM DHAwaitingFirstCardFSM))
        (BoutRels playerId dealerId roundId tableId (singletonFiniteMap Hand1 (Present testBoutId) Absent))

mkTestBoutDealerResolved :: PlayerId -> DealerId -> RoundId -> TableId -> EntityState 'Bout
mkTestBoutDealerResolved playerId dealerId roundId tableId =
    EBout
        (BoutAttrs testEmptyHand testEmptyHand Hand1 Absent)
        (BoutModes (SomeBoutFSM BDoneFSM) (SomePlayerHandFSM PHDecisionFSM) (SomeDealerHandFSM (DHResolvedFSM DHDealerStand)))
        (BoutRels playerId dealerId roundId tableId (singletonFiniteMap Hand1 (Present testBoutId) Absent))

runEvent :: BlackjackEvent -> SimState -> SimState
runEvent event state =
    let tick = simTick state
        Tick tickNum = tick
        nextTick = Tick (tickNum + 1)

        simEvent =
            SimEvent
                { eventId = EventId $ unTick tick
                , eventOccurred = Game event
                , eventTimestamp = tick
                , eventCausingIntent = Nothing
                }

        cache = mkCacheFromTrace (simTrace state) (simTick state)

        deltaResult =
            withTickCache cache $
                generateDeltas (Game event) $
                    CausalHistory Nothing (Just $ eventId simEvent)

        traceOps = case deltaResult of
            Right ops -> ops
            Left _errors -> []

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

runEventStrict :: BlackjackEvent -> SimState -> Either ValidationErrors SimState
runEventStrict event state =
    let tick = simTick state
        Tick tickNum = tick
        nextTick = Tick (tickNum + 1)

        simEvent =
            SimEvent
                { eventId = EventId $ unTick tick
                , eventOccurred = Game event
                , eventTimestamp = tick
                , eventCausingIntent = Nothing
                }

        cache = mkCacheFromTrace (simTrace state) (simTick state)

        deltaResult =
            withTickCache cache $
                generateDeltas (Game event) $
                    CausalHistory Nothing (Just $ eventId simEvent)
     in case deltaResult of
            Right traceOps ->
                let newTrace = foldl (flip $ \op' -> applyTraceOp op' tick) (simTrace state) traceOps
                    newEventLog =
                        EventLog $
                            IHM.insertWith
                                (++)
                                tick
                                [simEvent]
                                (eventLogEvents $ simEventLog state)
                 in Right $
                        state
                            { simTrace = newTrace
                            , simEventLog = newEventLog
                            , simTick = nextTick
                            }
            Left errors -> Left errors

mkInitialTraceWithOneCardDraw :: Tick -> Trace
mkInitialTraceWithOneCardDraw startTick =
    let dealerState =
            EDealer
                (DealerAttrs "Test Dealer" Absent)
                (DealerModes (SomeDealerTableFSM DOnDutyFSM) (PeekRound (SomePeekFSM PeekBoutPlayersFSM)))
                (DealerRels (Present testRoundId) (Present testTableId))

        boutState = mkTestBout testPlayerId testDealerId testRoundId testTableId

        roundState =
            ERound
                (RoundAttrs Absent 1)
                RoundModes
                (RoundRels (singletonFiniteMap TableSpot1 (Present testBoutId) Absent) testShoeId testTableId)

        -- Use mkS17Rules which has OneCardOnly for split aces
        oneCardDrawOffering =
            mkOffering
                (Materia D6 FaceUp)
                mkS17Rules
                (TableRuleSet 25 5000 2 AllowMidShoe (MultipliedMinBet 2.0) SingleCardBurn 0.75)

        tableState =
            ETable
                (TableAttrs "Test Table" oneCardDrawOffering (singletonFiniteMap TableSpot1 (Present (TableSeat testPlayerId False)) Absent))
                (TableModes (SomeTableFSM TRoundInProgressFSM))
                (TableRels (Present testDealerId) (Present testRoundId))

        shoeState =
            EShoe
                (ShoeAttrs [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts] mempty)
                ShoeModes
                (ShoeRels testTableId)

        trace0 = emptyTrace
        trace1 = applyTraceOp (bear testBoutId boutState) startTick trace0
        trace2 = applyTraceOp (bear testDealerId dealerState) startTick trace1
        trace3 = applyTraceOp (bear testRoundId roundState) startTick trace2
        trace4 = applyTraceOp (bear testTableId tableState) startTick trace3
        trace5 = applyTraceOp (bear testShoeId shoeState) startTick trace4
        trace6 = applyTraceOp (bear testPlayerId (mkTestPlayer "Test Player")) startTick trace5
     in trace6

mkInitialTrace :: Tick -> Trace
mkInitialTrace startTick =
    let dealerHandWithCards = characterize [Card Six Hearts]
        boutState =
            EBout
                (BoutAttrs testEmptyHand dealerHandWithCards Hand1 Absent)
                (BoutModes (SomeBoutFSM BPlayerTurnFSM) (SomePlayerHandFSM PHDecisionFSM) (SomeDealerHandFSM DHEvaluatingFSM))
                (BoutRels testPlayerId testDealerId testRoundId testTableId (singletonFiniteMap Hand1 (Present testBoutId) Absent))

        dealerState =
            EDealer
                (DealerAttrs "Test Dealer" Absent)
                (DealerModes (SomeDealerTableFSM DOnDutyFSM) (PeekRound (SomePeekFSM PeekBoutPlayersFSM)))
                (DealerRels (Present testRoundId) (Present testTableId))

        roundState =
            ERound
                (RoundAttrs Absent 1)
                RoundModes
                (RoundRels (singletonFiniteMap TableSpot1 (Present testBoutId) Absent) testShoeId testTableId)

        tableState =
            ETable
                (TableAttrs "Test Table" vegas6 (singletonFiniteMap TableSpot1 (Present (TableSeat testPlayerId False)) Absent))
                (TableModes (SomeTableFSM TRoundInProgressFSM))
                (TableRels (Present testDealerId) (Present testRoundId))

        shoeState =
            EShoe
                (ShoeAttrs [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts] mempty)
                ShoeModes
                (ShoeRels testTableId)

        playerState = mkTestPlayer "Test Player"

        trace0 = emptyTrace
        trace1 = applyTraceOp (bear testBoutId boutState) startTick trace0
        trace2 = applyTraceOp (bear testDealerId dealerState) startTick trace1
        trace3 = applyTraceOp (bear testRoundId roundState) startTick trace2
        trace4 = applyTraceOp (bear testTableId tableState) startTick trace3
        trace5 = applyTraceOp (bear testShoeId shoeState) startTick trace4
        trace6 = applyTraceOp (bear testPlayerId playerState) startTick trace5
     in trace6

loadCanonicalStrategy :: IO StrategyChart
loadCanonicalStrategy = handle handleError $ do
    baseline <- TIO.readFile "data/strategy/baseline.txt"
    overlay <- TIO.readFile "data/strategy/bja-basic.txt"
    case (parseStrategyChart baseline, parseStrategyChart overlay) of
        (Right baseChart, Right overlayChart) ->
            pure $ overlayStrategy baseChart overlayChart
        (Left errs, _) ->
            error $ "Failed to parse baseline strategy: " ++ show errs
        (_, Left errs) ->
            error $ "Failed to parse overlay strategy: " ++ show errs
  where
    handleError :: SomeException -> IO StrategyChart
    handleError ex = error $ "Failed to load strategy files: " ++ show ex

workingMistakeDistribution :: MistakeDistribution
workingMistakeDistribution =
    MistakeDistribution
        { _hitInsteadOfStand = 0.0
        , _standInsteadOfHit = 0.0
        , _noDoubleWhenShould = 0.0
        , _noSplitWhenShould = 0.0
        , _doubleWhenShouldnt = 0.0
        , _splitWhenShouldnt = 0.0
        }

mkTestBasicStrategy :: IO SomePlayerArchetype
mkTestBasicStrategy = do
    chart <- loadCanonicalStrategy
    pure $
        SomePlayerBasicStrategy $
            BasicStrategyArchetype
                (BasicConfig chart (MistakeProfile 0.0 workingMistakeDistribution))
                (BasicState 0 emptySessionStats)

mkS17Rules :: GameRuleSet
mkS17Rules =
    GameRuleSet
        { soft17 = StandSoft17
        , holeCardRule = Peek
        , das = DAS
        , doubling = DoubleAny
        , splitAcesAllowed = SplitAces
        , resplitAcesAllowed = NoResplitAces
        , splitAcesFrozen = OneCardOnly
        , splitHands = SP4
        , surrender = Late
        , payout = P3_2
        , pen = PenFrac 5 6
        }

mkH17Rules :: GameRuleSet
mkH17Rules = mkS17Rules{soft17 = HitSoft17}

buildSimStateFromEvents :: [LifecycleEvent] -> Tick -> SimState
buildSimStateFromEvents events startTick =
    let trace0 = emptyTrace

        processEvent :: (Trace, Tick) -> LifecycleEvent -> (Trace, Tick)
        processEvent (currentTrace, currentTick) event =
            let cache = mkCacheFromTrace currentTrace currentTick
                deltaResult =
                    withTickCache cache $
                        runExceptT $
                            generateLifecycleDeltas event $
                                CausalHistory Nothing Nothing

                traceOps = case deltaResult of
                    Right ops -> ops
                    Left _errors -> []

                newTrace = foldl (flip $ \op' -> applyTraceOp op' currentTick) currentTrace traceOps
                Tick tickNum = currentTick
                nextTick = Tick (tickNum + 1)
             in (newTrace, nextTick)

        (finalTrace, finalTick) = foldl processEvent (trace0, startTick) events
     in SimState
            { simTrace = finalTrace
            , simEventLog = EventLog mempty
            , simIntentLog = IntentLog mempty
            , simTick = finalTick
            }

buildSimStateFromMixedEvents :: [SimulationEvent] -> Tick -> SimState
buildSimStateFromMixedEvents events startTick =
    let trace0 = emptyTrace
        eventLog0 = EventLog mempty

        processEvent :: (Trace, EventLog, Tick) -> SimulationEvent -> (Trace, EventLog, Tick)
        processEvent (currentTrace, currentEventLog, currentTick) simulationEvent =
            let cache = mkCacheFromTrace currentTrace currentTick

                -- Create SimEvent for logging
                simEvent =
                    SimEvent
                        { eventId = EventId $ unTick currentTick
                        , eventOccurred = simulationEvent
                        , eventTimestamp = currentTick
                        , eventCausingIntent = Nothing
                        }

                -- Generate deltas based on event type
                deltaResult =
                    withTickCache cache $
                        generateDeltas simulationEvent $
                            CausalHistory Nothing (Just $ eventId simEvent)

                traceOps = case deltaResult of
                    Right ops -> ops
                    Left _errors -> []

                newTrace = foldl (flip $ \op' -> applyTraceOp op' currentTick) currentTrace traceOps

                -- Update event log
                newEventLog =
                    EventLog $
                        IHM.insertWith (++) currentTick [simEvent] (eventLogEvents currentEventLog)

                Tick tickNum = currentTick
                nextTick = Tick (tickNum + 1)
             in (newTrace, newEventLog, nextTick)

        (finalTrace, finalEventLog, finalTick) = foldl processEvent (trace0, eventLog0, startTick) events
     in SimState
            { simTrace = finalTrace
            , simEventLog = finalEventLog
            , simIntentLog = IntentLog mempty
            , simTick = finalTick
            }

-- Strict version that returns validation errors
buildSimStateFromMixedEventsStrict :: [SimulationEvent] -> Tick -> Either ValidationErrors SimState
buildSimStateFromMixedEventsStrict events startTick =
    let trace0 = emptyTrace
        eventLog0 = EventLog mempty

        processEvent :: (Trace, EventLog, Tick) -> SimulationEvent -> Either ValidationErrors (Trace, EventLog, Tick)
        processEvent (currentTrace, currentEventLog, currentTick) simulationEvent = do
            let cache = mkCacheFromTrace currentTrace currentTick

                -- Create SimEvent for logging
                simEvent =
                    SimEvent
                        { eventId = EventId $ unTick currentTick
                        , eventOccurred = simulationEvent
                        , eventTimestamp = currentTick
                        , eventCausingIntent = Nothing
                        }

                -- Generate deltas based on event type
                deltaResult =
                    withTickCache cache $
                        generateDeltas simulationEvent $
                            CausalHistory Nothing (Just $ eventId simEvent)

            traceOps <- deltaResult

            let newTrace = foldl (flip $ \op' -> applyTraceOp op' currentTick) currentTrace traceOps

                -- Update event log
                newEventLog =
                    EventLog $
                        IHM.insertWith (++) currentTick [simEvent] (eventLogEvents currentEventLog)

                Tick tickNum = currentTick
                nextTick = Tick (tickNum + 1)

            pure (newTrace, newEventLog, nextTick)

        foldResults :: Either ValidationErrors (Trace, EventLog, Tick) -> SimulationEvent -> Either ValidationErrors (Trace, EventLog, Tick)
        foldResults (Left err) _ = Left err
        foldResults (Right state) event = processEvent state event
     in case foldl foldResults (Right (trace0, eventLog0, startTick)) events of
            Left err -> Left err
            Right (finalTrace, finalEventLog, finalTick) ->
                Right $
                    SimState
                        { simTrace = finalTrace
                        , simEventLog = finalEventLog
                        , simIntentLog = IntentLog mempty
                        , simTick = finalTick
                        }

-- Helper functions for common entity birth events
playerCreated :: PlayerId -> String -> LifecycleEvent
playerCreated playerId name = EntityCreated PlayerWitness playerId (mkTestPlayer name)

dealerCreated :: DealerId -> String -> LifecycleEvent
dealerCreated dealerId name = EntityCreated DealerWitness dealerId (mkTestDealer name)

boutCreated :: BoutId -> PlayerId -> DealerId -> RoundId -> TableId -> LifecycleEvent
boutCreated boutId playerId dealerId roundId tableId =
    EntityCreated BoutWitness boutId (mkTestBout playerId dealerId roundId tableId)

tableCreated :: TableId -> String -> Offering -> LifecycleEvent
tableCreated tableId name offering =
    let tableState =
            ETable
                (TableAttrs name offering (singletonFiniteMap TableSpot1 (Present (TableSeat testPlayerId False)) Absent))
                (TableModes (SomeTableFSM TRoundInProgressFSM))
                (TableRels (Present testDealerId) (Present testRoundId))
     in EntityCreated TableWitness tableId tableState

roundCreated :: RoundId -> ShoeId -> TableId -> LifecycleEvent
roundCreated roundId shoeId tableId =
    let roundState =
            ERound
                (RoundAttrs Absent 1)
                RoundModes
                (RoundRels (singletonFiniteMap TableSpot1 (Present testBoutId) Absent) shoeId tableId)
     in EntityCreated RoundWitness roundId roundState

shoeCreated :: ShoeId -> TableId -> [Card] -> LifecycleEvent
shoeCreated shoeId tableId cards =
    let shoeState =
            EShoe
                (ShoeAttrs cards mempty)
                ShoeModes
                (ShoeRels tableId)
     in EntityCreated ShoeWitness shoeId shoeState

basicGameSetupEvents :: [LifecycleEvent]
basicGameSetupEvents =
    [ playerCreated testPlayerId "Test Player"
    , dealerCreated testDealerId "Test Dealer"
    , tableCreated testTableId "Test Table" vegas6
    , roundCreated testRoundId testShoeId testTableId
    , shoeCreated testShoeId testTableId [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts]
    , boutCreated testBoutId testPlayerId testDealerId testRoundId testTableId
    , EntityMutated BoutWitness testBoutId (AttrsDelta emptyCausalHistory (DBoutSetDealerHand (characterize [Card Six Hearts]) testEmptyHand))
    ]

lifecycleToSimulation :: LifecycleEvent -> SimulationEvent
lifecycleToSimulation = Lifecycle

basicGameSetupMixedEvents :: [SimulationEvent]
basicGameSetupMixedEvents = map lifecycleEvent basicGameSetupEvents

emptyHandsGameSetupEvents :: [LifecycleEvent]
emptyHandsGameSetupEvents =
    [ playerCreated testPlayerId "Test Player"
    , dealerCreated testDealerId "Test Dealer"
    , tableCreated testTableId "Test Table" vegas6
    , roundCreated testRoundId testShoeId testTableId
    , shoeCreated testShoeId testTableId [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts]
    , boutCreated testBoutId testPlayerId testDealerId testRoundId testTableId
    ]

emptyHandsGameSetupMixedEvents :: [SimulationEvent]
emptyHandsGameSetupMixedEvents = map lifecycleEvent emptyHandsGameSetupEvents

gameEvent :: BlackjackEvent -> SimulationEvent
gameEvent = Game

economyEvent :: EconomyEvent -> SimulationEvent
economyEvent = Economy

lifecycleEvent :: LifecycleEvent -> SimulationEvent
lifecycleEvent = Lifecycle

playerDealtCard :: Card -> SimulationEvent
playerDealtCard card = gameEvent (BoutPlayerCardDealt card testBoutId)

dealerDealtCard :: Card -> SimulationEvent
dealerDealtCard card = gameEvent (BoutDealerCardDealt card testBoutId)

playerHandSet :: [Card] -> SimulationEvent
playerHandSet cards = gameEvent (BoutPlayerHandSet (characterize cards) testBoutId)

dealerHandSet :: [Card] -> SimulationEvent
dealerHandSet cards = gameEvent (BoutDealerHandSet (characterize cards) testBoutId)

playerStands :: SimulationEvent
playerStands = gameEvent (BoutPlayerStood testBoutId)

playerHits :: SimulationEvent
playerHits = gameEvent (BoutPlayerHit testBoutId)

playerDoubles :: SimulationEvent
playerDoubles = gameEvent (BoutPlayerDoubledDown testBoutId)

playerSplits :: SimulationEvent
playerSplits = gameEvent (BoutPlayerSplit testBoutId)

playerSurrenders :: SimulationEvent
playerSurrenders = gameEvent (BoutPlayerSurrendered testBoutId)
