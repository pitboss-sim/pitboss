{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pitboss.Unit.Serialization.JsonSpec where

import Data.Aeson
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Map.Strict qualified as Map
import Test.Hspec
import Test.QuickCheck

import Control.Monad (forM_)
import Pitboss.Blackjack
import Pitboss.Blackjack.Strategy.Chart.Types
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Test.Pitboss.TestUtils

-- Test helper
roundtrips :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Bool
roundtrips x = decode (encode x) == Just x

spec :: Spec
spec = describe "JSON Roundtrip Tests" $ do
    describe "Core Types" $ do
        it "Card roundtrips" $ property $ \card ->
            roundtrips (card :: Card)

        it "Chips roundtrips" $ property $ \chips ->
            roundtrips (chips :: Chips)

        it "EntityId roundtrips" $ property $ \eid ->
            roundtrips (eid :: EntityId 'Player)

        it "Tick roundtrips" $ property $ \tick ->
            roundtrips (tick :: Tick)

        it "IntentId roundtrips" $ property $ \iid ->
            roundtrips (iid :: IntentId)

        it "EventId roundtrips" $ property $ \eid ->
            roundtrips (eid :: EventId)

        it "CausalHistory roundtrips" $ property $ \ch ->
            roundtrips (ch :: CausalHistory)

    describe "Hand Types" $ do
        it "SomeHand roundtrips for all hand types" $ do
            let testHands =
                    [ characterize [] -- Empty hand
                    , characterize [Card Ace Hearts, Card King Spades] -- Blackjack
                    , characterize [Card Ace Hearts, Card Six Spades] -- Soft 17
                    , characterize [Card Ten Hearts, Card Seven Spades] -- Hard 17
                    , characterize [Card Eight Hearts, Card Eight Spades] -- Pair
                    , characterize [Card Ten Hearts, Card Six Spades, Card King Clubs] -- Bust
                    ]
            forM_ testHands $ \hand ->
                roundtrips hand `shouldBe` True

    describe "FSM Types" $ do
        it "SomePlayerHandFSM roundtrips" $ do
            let fsms =
                    [ SomePlayerHandFSM PHDecisionFSM
                    , SomePlayerHandFSM PHHittingFSM
                    , SomePlayerHandFSM PHBlackjackFSM
                    , SomePlayerHandFSM (PHOneCardDrawFSM PHDouble)
                    , SomePlayerHandFSM (PHResolvedFSM PHStand)
                    , SomePlayerHandFSM (PHAbandonedFSM (PHSurrender Late))
                    ]
            forM_ fsms $ \fsm ->
                roundtrips fsm `shouldBe` True

        it "SomeDealerHandFSM roundtrips" $ do
            let fsms =
                    [ SomeDealerHandFSM DHDealingFSM
                    , SomeDealerHandFSM DHEvaluatingFSM
                    , SomeDealerHandFSM (DHResolvedFSM DHDealerStand)
                    , SomeDealerHandFSM (DHInterruptedFSM Banking)
                    ]
            forM_ fsms $ \fsm ->
                roundtrips fsm `shouldBe` True

        it "SomePlayerTableFSM roundtrips" $ do
            let fsms =
                    [ SomePlayerTableFSM PTIdleFSM
                    , SomePlayerTableFSM PTChoosingTableFSM
                    , SomePlayerTableFSM PTPlacingBetFSM
                    , SomePlayerTableFSM PTPlayingHandFSM
                    , SomePlayerTableFSM PTObservingFSM
                    , SomePlayerTableFSM PTDoneFSM
                    ]
            forM_ fsms $ \fsm ->
                roundtrips fsm `shouldBe` True

        it "DealerRoundFSM roundtrips" $ do
            let fsms =
                    [ PeekDealerRound (SomePeekFSM PeekAwaitingFSM)
                    , PeekDealerRound (SomePeekFSM PeekPlayersFSM)
                    , ENHCDealerRound (SomeENHCFSM ENHCAwaitingFSM)
                    , ENHCDealerRound (SomeENHCFSM ENHCPlayersFSM)
                    ]
            forM_ fsms $ \fsm ->
                roundtrips fsm `shouldBe` True

    describe "Entity States" $ do
        it "EPlayer roundtrips" $ do
            let player =
                    EPlayer
                        { _pAttrs = PlayerAttrs "Test Player" (Chips 1000)
                        , _pModes =
                            PlayerModes
                                (SomePlayerTableFSM PTIdleFSM)
                                (SomePlayerSpotFSM PSIdleFSM)
                                (SomePlayerHandFSM PHDecisionFSM)
                        , _pRels = PlayerRels
                        }
            roundtrips player `shouldBe` True

        it "EPlayerHand roundtrips" $ do
            let hand =
                    EPlayerHand
                        { _phAttrs =
                            PlayerHandAttrs
                                { _phAttrsHand = characterize [Card Ten Hearts, Card Nine Spades]
                                , _phAttrsOriginalBet = Chips 100
                                , _phAttrsSplitDepth = 0
                                , _phAttrsHandIx = 0
                                }
                        , _phModes = PlayerHandModes (SomePlayerHandFSM PHDecisionFSM)
                        , _phRels =
                            PlayerHandRels
                                (EntityId 600)
                                (EntityId 700)
                                (EntityId 100)
                                (EntityId 300)
                        }
            roundtrips hand `shouldBe` True

        it "EBout roundtrips" $ do
            let bout =
                    EBout
                        { _boutAttrs = BoutAttrs (Just pushOutcome)
                        , _boutModes = BoutModes (SomeBoutFSM BPlayerTurnFSM)
                        , _boutRels =
                            BoutRels
                                (EntityId 400)
                                (EntityId 500)
                                (EntityId 900)
                                (EntityId 800)
                                (EntityId 700)
                        }
            roundtrips bout `shouldBe` True

    describe "Delta Types" $ do
        it "SomeDelta 'PlayerHand roundtrips" $ do
            let history = CausalHistory (Just (IntentId 123)) (Just (EventId 456))
            let deltas =
                    [ AttrsDelta
                        history
                        ( DPlayerHandSetHand
                            (characterize [Card King Hearts])
                            (characterize [])
                        )
                    , ModesDelta
                        history
                        ( DPlayerHandSetPlayerHandFSM
                            (SomePlayerHandFSM PHHittingFSM)
                            (SomePlayerHandFSM PHDecisionFSM)
                        )
                    , RelsDelta
                        history
                        ( DPlayerHandSetPlayerSpot
                            (EntityId 601)
                            (EntityId 600)
                        )
                    ]
            forM_ deltas $ \delta ->
                roundtrips delta `shouldBe` True

    describe "Event Types" $ do
        it "BlackjackEvent roundtrips" $ do
            let events =
                    [ CardDealt (Card Ace Hearts) (ToPlayerHand (EntityId 400))
                    , CardDealt (Card King Spades) (ToDealerHand (EntityId 500))
                    , PlayerStood (EntityId 100) (EntityId 400)
                    , PlayerHit (EntityId 100) (EntityId 400)
                    , PlayerDoubledDown (EntityId 100) (EntityId 400)
                    , PlayerSplit (EntityId 100) (EntityId 400)
                    , PlayerSurrender (EntityId 100) (EntityId 400)
                    , BoutSettled (EntityId 300) dealerWinsHigher
                    , DealerRevealed (EntityId 200) (EntityId 500)
                    ]
            forM_ events $ \event ->
                roundtrips event `shouldBe` True

    describe "Agent Types" $ do
        it "SomePlayerArchetype roundtrips" $ do
            chart <- loadCanonicalStrategy
            let archetypes =
                    [ SomePlayerBasicStrategy $
                        BasicStrategyArchetype
                            { bsConfig = BasicConfig chart (MistakeProfile 0.05 workingMistakeDistribution)
                            , bsState = BasicState 10 emptySessionStats
                            }
                    , SomePlayerPerfect $
                        PerfectArchetype
                            { pfConfig = PerfectConfig True
                            , pfState = PerfectState 5 emptySessionStats
                            }
                    , SomePlayerAdvantage $
                        AdvantageArchetype
                            { advConfig = AdvantageConfig HiLo FlatBetting (DeviationChart Map.empty)
                            , advState = AdvantageState 0 0.0 6.0 emptySessionStats
                            }
                    , SomePlayerSuperstitious $
                        SuperstitiousArchetype
                            { ssConfig = SuperstitionConfig [NeverHitOn16, AlwaysSplitAces] chart
                            , ssState = SuperstitionState 20 3 emptySessionStats
                            }
                    ]
            forM_ archetypes $ \archetype ->
                roundtrips archetype `shouldBe` True

    describe "Complex Types" $ do
        it "Registry roundtrips" $ do
            let timeline = mkTimeline (EntityId 100) (Tick 1000) (EPlayer (PlayerAttrs "Test" (Chips 1000)) (PlayerModes (SomePlayerTableFSM PTIdleFSM) (SomePlayerSpotFSM PSIdleFSM) (SomePlayerHandFSM PHDecisionFSM)) PlayerRels)
            let reg = Registry (IHM.singleton (EntityId 100) timeline) :: Registry 'Player (SomeDelta 'Player)
            roundtrips reg `shouldBe` True

        it "FiniteMap roundtrips" $ do
            let fm = singletonFiniteMap EPlayerSpotHand1 (Present (EntityId 400)) Absent
            roundtrips fm `shouldBe` True

    describe "Edge Cases" $ do
        it "handles empty collections" $ do
            roundtrips (EventLog IHM.empty) `shouldBe` True
            roundtrips (IntentLog IHM.empty) `shouldBe` True

        it "handles nested Maybe values" $ do
            roundtrips (DealerRels Nothing Nothing Nothing) `shouldBe` True
            roundtrips (DealerRels (Just (EntityId 800)) Nothing (Just (EntityId 500))) `shouldBe` True

        it "handles special numeric values" $ do
            roundtrips (Tick 0) `shouldBe` True
            roundtrips (Tick maxBound) `shouldBe` True
            roundtrips (Chips 0) `shouldBe` True
            roundtrips (Chips (-100)) `shouldBe` True

        describe "Game Rules Types" $ do
            it "GameRuleSet roundtrips" $ do
                let rulesets =
                        [ mkS17Rules -- from DealerRulesSpec
                        , mkH17Rules
                        , GameRuleSet
                            ENHC
                            HitSoft17
                            NoDAS
                            Double10_11
                            NoSplitAces
                            NoResplitAces
                            OneCardOnly
                            SP2
                            NoSurrender
                            P6_5
                            (PenCards 50)
                        ]
                forM_ rulesets $ \rules ->
                    roundtrips rules `shouldBe` True

            it "Offering roundtrips" $ do
                let offerings = [vegas6, downtownSingleDeck]
                forM_ offerings $ \offering ->
                    roundtrips offering `shouldBe` True

        describe "Strategy Chart Types" $ do
            it "ChartEntry roundtrips" $ do
                let entry =
                        ChartEntry
                            { handKind = HardHand
                            , kindValue = Just 16
                            , moves = Map.fromList [(Two, MoveHit), (Ten, MoveSurrenderOrStand)]
                            }
                roundtrips entry `shouldBe` True

            it "MoveCode roundtrips" $ property $ \(n :: Int) ->
                -- Generate arbitrary MoveCode
                let codes =
                        [ MoveHit
                        , MoveStand
                        , MoveDoubleOrHit
                        , MoveDoubleOrStand
                        , MoveSplit
                        , MoveSplitOrHit
                        , MoveSurrenderOrStand
                        , MoveUndefined
                        ]
                    arbitraryCode = codes !! (abs n `mod` length codes)
                 in roundtrips arbitraryCode

        describe "More Delta Types" $ do
            it "All entity delta types roundtrip" $ do
                let history = CausalHistory (Just (IntentId 123)) (Just (EventId 456))

                -- Dealer deltas
                let dealerDeltas =
                        [ AttrsDelta history (DDealerSetName "New" "Old")
                        , ModesDelta
                            history
                            ( DDealerSetTableFSM
                                (SomeDealerTableFSM DTOnDutyFSM)
                                (SomeDealerTableFSM DTOffDutyFSM)
                            )
                        , RelsDelta
                            history
                            ( DDealerSetActiveTable
                                (Just (EntityId 800))
                                Nothing
                            )
                        ]
                forM_ dealerDeltas $ \delta ->
                    roundtrips delta `shouldBe` True

                -- Table deltas
                let tableDeltas =
                        [ AttrsDelta history (DTableSetName "Table2" "Table1")
                        , AttrsDelta history (DTableSetOffering downtownSingleDeck vegas6)
                        , RelsDelta history (DTableSetDealer (Just (EntityId 200)) Nothing)
                        ]
                forM_ tableDeltas $ \delta ->
                    roundtrips delta `shouldBe` True

        describe "Trace Types" $ do
            -- it "Trace roundtrips with populated registries" $ do
            --     let trace = mkInitialTrace (Tick 1000)
            --         in
            --         roundtrips trace `shouldBe` True

            it "TraceOp roundtrips" $ do
                player <- mkTestPlayer "Test"
                let playerHand = mkTestPlayerHand (EntityId 600) (EntityId 700) (EntityId 100) (EntityId 300)
                let bout = mkTestBout (EntityId 400) (EntityId 500) (EntityId 900) (EntityId 800) (EntityId 700)
                let ops =
                        [ createBirth (EntityId 100) player
                        , createMutation
                            (EntityId 100)
                            player
                            ( AttrsDelta
                                (CausalHistory Nothing Nothing)
                                (DPlayerSetBankroll (Chips 2000) (Chips 1000))
                            )
                        , createBirth (EntityId 400) playerHand
                        , createDeath (EntityId 400) playerHand (HandResolved PHBust)
                        , createBirth (EntityId 300) bout
                        , createDeath (EntityId 300) bout (BoutComplete pushOutcome)
                        ]
                forM_ ops $ \op ->
                    roundtrips op `shouldBe` True

        describe "Timeline Types" $ do
            it "Timeline with deltas roundtrips" $ do
                player <- mkTestPlayer "Test" -- this is in IO
                let timeline =
                        Timeline
                            { timelineMeta = Meta (EntityId 100) (Tick 1000) Nothing
                            , timelineInitialState = Just player -- now we have the player
                            , timelineDeltas =
                                IHM.singleton
                                    (Tick 1001)
                                    [ AttrsDelta
                                        (CausalHistory Nothing Nothing)
                                        (DPlayerSetBankroll (Chips 900) (Chips 1000))
                                    ]
                            }
                roundtrips timeline `shouldBe` True
