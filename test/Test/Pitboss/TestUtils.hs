{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Pitboss.TestUtils where

import Control.Exception (handle)
import Control.Monad.Reader (Reader)
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Text.IO qualified as TIO
import GHC.Exception (SomeException)
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Causality.Validate
import Pitboss.FSM
import Pitboss.Simulation
import Test.QuickCheck (Arbitrary (..), elements, oneof)

testTick :: Tick
testTick = Tick 1000

testPlayerId :: PlayerId
testPlayerId = PlayerId 1200

testContestantId :: ContestantId
testContestantId = ContestantId 100

testDealerId :: DealerId
testDealerId = DealerId 200

testBoutId :: BoutId
testBoutId = BoutId 300

testHandId :: HandId
testHandId = HandId 400

testDealerHandId :: HandId
testDealerHandId = HandId 500

testRoundId :: RoundId
testRoundId = RoundId 700

testTableId :: TableId
testTableId = TableId 800

testShoeId :: ShoeId
testShoeId = ShoeId 900

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

instance Arbitrary ContestantId where
    arbitrary = ContestantId . abs <$> arbitrary

instance Arbitrary DealerId where
    arbitrary = DealerId . abs <$> arbitrary

instance Arbitrary BoutId where
    arbitrary = BoutId . abs <$> arbitrary

instance Arbitrary HandId where
    arbitrary = HandId . abs <$> arbitrary

instance Arbitrary RoundId where
    arbitrary = RoundId . abs <$> arbitrary

instance Arbitrary TableId where
    arbitrary = TableId . abs <$> arbitrary

instance Arbitrary ShoeId where
    arbitrary = ShoeId . abs <$> arbitrary

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
            [ pure contestantWinsHigher
            , pure contestantWinsDealerBust
            , pure contestantWinsBlackjack
            , pure dealerWinsHigher
            , pure dealerWinsContestantBust
            , pure dealerWinsBlackjack
            , pure pushOutcome
            ]

mkCacheFromTrace :: Trace -> Tick -> TickCache
mkCacheFromTrace trace =
    populateTickCache
        (_bouts trace)
        (_contestants trace)
        (_dealers trace)
        (_hands trace)
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
        (DealerAttrs name)
        (DealerModes (SomeDealerTableFSM DTOffDutyFSM) (PeekRound (SomePeekFSM PeekAwaitingFSM)))
        (DealerRels Absent Absent Absent)

mkTestContestant :: PlayerId -> RoundId -> EntityState 'Contestant
mkTestContestant playerId roundId =
    EContestant
        (ContestantAttrs Hand1)
        ( ContestantModes
            (Present (SomeContestantBoutFSM CBPlayingFSM))
            (SomeContestantRoundFSM CREngagedFSM)
        )
        (ContestantRels (singletonFiniteMap Hand1 (Present testBoutId) Absent) playerId roundId testShoeId)

mkTestContestantAwaitingCards :: PlayerId -> RoundId -> EntityState 'Contestant
mkTestContestantAwaitingCards playerId roundId =
    EContestant
        (ContestantAttrs Hand1)
        ( ContestantModes
            (Present (SomeContestantBoutFSM CBAwaitingCardsFSM))
            (SomeContestantRoundFSM CREngagedFSM)
        )
        (ContestantRels (singletonFiniteMap Hand1 (Present testBoutId) Absent) playerId roundId testShoeId)

mkTestContestantResolved :: ContestantHandResolution -> PlayerId -> RoundId -> EntityState 'Contestant
mkTestContestantResolved _resolution playerId roundId =
    EContestant
        (ContestantAttrs Hand1)
        ( ContestantModes
            (Present (SomeContestantBoutFSM CBResolvedFSM))
            (SomeContestantRoundFSM CRResolvedFSM)
        )
        (ContestantRels (singletonFiniteMap Hand1 (Present testBoutId) Absent) playerId roundId testShoeId)

mkTestBout :: HandId -> HandId -> RoundId -> ShoeId -> TableId -> EntityState 'Bout
mkTestBout playerHandId dealerHandId roundId shoeId tableId =
    EBout
        (BoutAttrs Absent)
        (BoutModes (SomeBoutFSM BAwaitingFirstCardFSM))
        (BoutRels playerHandId dealerHandId roundId shoeId tableId)

mkTestContestantHand :: ContestantId -> EntityState 'Hand
mkTestContestantHand contestantId =
    mkTestHandWithFSM (ContestantOwner contestantId) (characterize []) (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM))

mkTestContestantHandAwaitingFirst :: ContestantId -> EntityState 'Hand
mkTestContestantHandAwaitingFirst contestantId =
    mkTestHandWithFSM (ContestantOwner contestantId) (characterize []) (ContestantHandFSM (SomeContestantHandFSM CHAwaitingFirstCardFSM))

mkTestContestantHandAwaitingSecond :: ContestantId -> EntityState 'Hand
mkTestContestantHandAwaitingSecond contestantId =
    mkTestHandWithFSM (ContestantOwner contestantId) (characterize []) (ContestantHandFSM (SomeContestantHandFSM CHAwaitingSecondCardFSM))

mkTestContestantHandHitting :: ContestantId -> EntityState 'Hand
mkTestContestantHandHitting contestantId =
    mkTestHandWithFSM (ContestantOwner contestantId) (characterize []) (ContestantHandFSM (SomeContestantHandFSM CHHittingFSM))

mkTestContestantHandDoubling :: ContestantId -> EntityState 'Hand
mkTestContestantHandDoubling contestantId =
    mkTestHandWithFSM (ContestantOwner contestantId) (characterize []) (ContestantHandFSM (SomeContestantHandFSM (CHAwaitingOneCardFSM OCDouble)))

mkTestContestantHandResolved :: ContestantHandResolution -> ContestantId -> EntityState 'Hand
mkTestContestantHandResolved resolution contestantId =
    mkTestHandWithFSM (ContestantOwner contestantId) (characterize []) (ContestantHandFSM (SomeContestantHandFSM (CHResolvedFSM resolution)))

mkTestDealerHand :: DealerId -> EntityState 'Hand
mkTestDealerHand dealerId =
    mkTestHandWithFSM (DealerOwner dealerId) (characterize []) (DealerHandFSM (SomeDealerHandFSM DHDealingFSM))

mkTestDealerHandRevealed :: DealerId -> EntityState 'Hand
mkTestDealerHandRevealed dealerId =
    mkTestHandWithFSM (DealerOwner dealerId) (characterize []) (DealerHandFSM (SomeDealerHandFSM DHEvaluatingFSM))

mkTestDealerHandResolved :: DealerHandResolution -> DealerId -> EntityState 'Hand
mkTestDealerHandResolved resolution dealerId =
    mkTestHandWithFSM (DealerOwner dealerId) (characterize []) (DealerHandFSM (SomeDealerHandFSM (DHResolvedFSM resolution)))

mkTestHand :: HandOwner -> SomeHand -> EntityState 'Hand
mkTestHand owner hand =
    let defaultFSM = case owner of
            ContestantOwner _ -> ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM)
            DealerOwner _ -> DealerHandFSM (SomeDealerHandFSM DHDealingFSM)
     in mkTestHandWithFSM owner hand defaultFSM

mkTestHandWithFSM :: HandOwner -> SomeHand -> SomeHandFSM -> EntityState 'Hand
mkTestHandWithFSM owner hand fsm =
    EHand
        (HandAttrs hand)
        (HandModes fsm)
        (HandRels owner testBoutId testShoeId)

runEvent :: BlackjackEvent -> SimState -> SimState
runEvent event state =
    let tick = simTick state
        Tick tickNum = tick
        nextTick = Tick (tickNum + 1)

        simEvent =
            SimEvent
                { eventId = EventId $ unTick tick
                , eventOccurred = event
                , eventTimestamp = tick
                , eventCausingIntent = Nothing
                }

        cache = mkCacheFromTrace (simTrace state) (simTick state)

        deltaResult =
            withTickCache cache $
                generateDeltas event $
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
                , eventOccurred = event
                , eventTimestamp = tick
                , eventCausingIntent = Nothing
                }

        cache = mkCacheFromTrace (simTrace state) (simTick state)

        deltaResult =
            withTickCache cache $
                generateDeltas event $
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

mkInitialTrace :: Tick -> Trace
mkInitialTrace startTick =
    let contestantState = mkTestContestant testPlayerId testRoundId

        dealerState =
            EDealer
                (DealerAttrs "Test Dealer")
                (DealerModes (SomeDealerTableFSM DTOnDutyFSM) (PeekRound (SomePeekFSM PeekContestantsFSM)))
                (DealerRels (Present testDealerHandId) (Present testRoundId) (Present testTableId))

        boutState = mkTestBout testHandId testDealerHandId testRoundId testShoeId testTableId

        contestantHandState = mkTestHandWithFSM (ContestantOwner testContestantId) (characterize []) (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM))

        dealerHandState =
            EHand
                (HandAttrs (characterize [Card Ten Diamonds]))
                (HandModes (DealerHandFSM (SomeDealerHandFSM DHDealingFSM)))
                (HandRels (DealerOwner testDealerId) testBoutId testShoeId)

        roundState =
            ERound
                (RoundAttrs Absent 1)
                RoundModes
                (RoundRels (Present testDealerHandId) (singletonFiniteMap TableSpot1 (Present testContestantId) Absent) testShoeId testTableId)

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

        trace0 = emptyTrace
        trace1 = applyTraceOp (createBirth testContestantId contestantState) startTick trace0
        trace2 = applyTraceOp (createBirth testDealerId dealerState) startTick trace1
        trace3 = applyTraceOp (createBirth testBoutId boutState) startTick trace2
        trace4 = applyTraceOp (createBirth testDealerHandId dealerHandState) startTick trace3
        trace5 = applyTraceOp (createBirth testHandId contestantHandState) startTick trace4
        trace6 = applyTraceOp (createBirth testRoundId roundState) startTick trace5
        trace7 = applyTraceOp (createBirth testTableId tableState) startTick trace6
        trace8 = applyTraceOp (createBirth testShoeId shoeState) startTick trace7
     in trace8

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
