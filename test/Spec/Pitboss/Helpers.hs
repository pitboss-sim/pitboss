{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.Pitboss.Helpers where

import Control.Exception (SomeException, handle)
import Control.Monad.Reader (Reader)
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Text.IO qualified as TIO
import Pitboss.Blackjack
import Pitboss.Blackjack.Strategy.Chart
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Test.QuickCheck (Arbitrary (..), elements, oneof)

-- QuickCheck generators
instance Arbitrary Rank where
    arbitrary = elements [Two .. Ace]

instance Arbitrary Suit where
    arbitrary = elements [Hearts .. Spades]

instance Arbitrary Card where
    arbitrary = Card <$> arbitrary <*> arbitrary

instance Arbitrary Chips where
    arbitrary = Chips . abs <$> arbitrary

instance Arbitrary (EntityId k) where
    arbitrary = EntityId . abs <$> arbitrary

instance Arbitrary Tick where
    arbitrary = Tick . abs <$> arbitrary

instance Arbitrary IntentId where
    arbitrary = IntentId . abs <$> arbitrary

instance Arbitrary EventId where
    arbitrary = EventId . abs <$> arbitrary

instance Arbitrary CausalHistory where
    arbitrary = CausalHistory <$> arbitrary <*> arbitrary

instance Arbitrary PlayerSpotIx where
    arbitrary = elements [EPlayerSpot1 .. EPlayerSpot4]

instance Arbitrary PlayerSpotHandIx where
    arbitrary = elements [EPlayerSpotHand1 .. EPlayerSpotHand4]

instance Arbitrary DetailedOutcome where
    arbitrary =
        oneof
            [ pure playerWinsHigher
            , pure playerWinsDealerBust
            , pure playerWinsBlackjack
            , pure dealerWinsHigher
            , pure dealerWinsPlayerBust
            , pure dealerWinsBlackjack
            , pure pushOutcome
            ]

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
                { bsConfig = BasicConfig chart (MistakeProfile 0.0 workingMistakeDistribution)
                , bsState = BasicState 0 emptySessionStats
                }

mkTestDealerArchetype :: SomeDealerArchetype
mkTestDealerArchetype =
    SomeDealerByTheBook $
        ByTheBookDealerArchetype
            { btbConfig = ByTheBookConfig (PenetrationProfile 0.75 0.05) (PaceProfile 100 10.0)
            , btbState = ByTheBookState 0
            }

mkTestPlayer :: String -> IO (EntityState 'Player)
mkTestPlayer name =
    pure $
        EPlayer
            { _pAttrs =
                PlayerAttrs
                    { _pAttrsName = name
                    , _pAttrsBankroll = Chips 1000
                    }
            , _pModes =
                PlayerModes
                    { _pModesPlayerTable = SomePlayerTableFSM PTIdleFSM
                    , _pModesPlayerSpot = SomePlayerSpotFSM PSIdleFSM
                    , _pModesPlayerHand = SomePlayerHandFSM PHDecisionFSM
                    }
            , _pRels = PlayerRels
            }

mkTestDealer :: String -> EntityState 'Dealer
mkTestDealer name =
    EDealer
        { _dAttrs =
            DealerAttrs
                { _dAttrsName = name
                }
        , _dModes =
            DealerModes
                { _dModesDealerTable = SomeDealerTableFSM DTOffDutyFSM
                , _dModesDealerRound = PeekDealerRound (SomePeekFSM PeekAwaitingFSM)
                , _dModesDealerHand = SomeDealerHandFSM DHDealingFSM
                }
        , _dRels = DealerRels Nothing Nothing Nothing
        }

mkTestPlayerHand ::
    EntityId 'PlayerSpot ->
    EntityId 'DealerRound ->
    EntityId 'Player ->
    EntityId 'Bout ->
    EntityState 'PlayerHand
mkTestPlayerHand spotId roundId playerId boutId =
    EPlayerHand
        { _phAttrs =
            PlayerHandAttrs
                { _phAttrsHand = characterize []
                , _phAttrsOriginalBet = Chips 100
                , _phAttrsSplitDepth = 0
                , _phAttrsHandIx = 0
                }
        , _phModes = PlayerHandModes (SomePlayerHandFSM PHDecisionFSM)
        , _phRels = PlayerHandRels spotId roundId playerId boutId
        }

mkTestDealerHand :: EntityId 'DealerRound -> EntityId 'Dealer -> EntityState 'DealerHand
mkTestDealerHand roundId dealerId =
    EDealerHand
        { _dhAttrs = DealerHandAttrs (characterize [])
        , _dhModes = DealerHandModes (SomeDealerHandFSM DHDealingFSM)
        , _dhRels = DealerHandRels roundId dealerId
        }

mkTestBout :: EntityId 'PlayerHand -> EntityId 'DealerHand -> EntityId 'TableShoe -> EntityId 'Table -> EntityId 'DealerRound -> EntityState 'Bout
mkTestBout playerHandId dealerHandId shoeId tableId roundId =
    EBout
        { _boutAttrs = BoutAttrs Nothing
        , _boutModes = BoutModes (SomeBoutFSM BAwaitingFirstCardFSM)
        , _boutRels = BoutRels playerHandId dealerHandId shoeId tableId roundId
        }

createPlayerHandForBout ::
    EntityId 'PlayerHand ->
    EntityId 'PlayerSpot ->
    EntityId 'DealerRound ->
    EntityId 'Player ->
    EntityId 'Bout ->
    Chips ->
    EntityState 'PlayerHand
createPlayerHandForBout _handId spotId roundId playerId boutId wager =
    EPlayerHand
        { _phAttrs =
            PlayerHandAttrs
                { _phAttrsHand = characterize []
                , _phAttrsOriginalBet = wager
                , _phAttrsSplitDepth = 0
                , _phAttrsHandIx = 0
                }
        , _phModes = PlayerHandModes (SomePlayerHandFSM PHDecisionFSM)
        , _phRels =
            PlayerHandRels
                { _phRelsBelongsToPlayerSpot = spotId
                , _phRelsBelongsToDealerRound = roundId
                , _phRelsOwnedByPlayer = playerId
                , _phRelsBelongsToBout = boutId
                }
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
                generateDeltas event $
                    CausalHistory Nothing (Just $ eventId simEvent)

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
