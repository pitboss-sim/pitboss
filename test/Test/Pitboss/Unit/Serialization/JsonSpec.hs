{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pitboss.Unit.Serialization.JsonSpec where

import Control.Monad (forM_)
import Data.Aeson (decode, encode)
import Data.Aeson.Types
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Map.Strict qualified as Map
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.TestUtils

roundtrips :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
roundtrips x = decode (encode x) == Just x

spec :: Spec
spec = describe "JSON Roundtrip Tests" $ do
    describe "Hand Types" $ do
        it "SomeHand roundtrips for all hand types" $ do
            let testHands =
                    [ characterize [Card Ace Hearts, Card King Spades]
                    , characterize [Card Ace Hearts, Card Six Spades]
                    , characterize [Card Ten Hearts, Card Seven Spades]
                    , characterize [Card Eight Hearts, Card Eight Spades]
                    , characterize [Card Ten Hearts, Card Six Spades, Card King Clubs]
                    ]
            forM_ testHands $ \hand ->
                roundtrips hand `shouldBe` True

    describe "FSM Types" $ do
        it "FSM roundtrips" $ do
            roundtrips (SomePlayerHandFSM PHDecisionFSM) `shouldBe` True
            roundtrips (SomePlayerHandFSM (PHResolvedFSM PHStand)) `shouldBe` True
            roundtrips (SomeDealerHandFSM DHAwaitingFirstCardFSM) `shouldBe` True
            roundtrips (SomeDealerHandFSM DHAwaitingSecondCardFSM) `shouldBe` True
            roundtrips (SomeDealerHandFSM DHDealingFSM) `shouldBe` True
            roundtrips (SomeDealerHandFSM (DHResolvedFSM DHDealerStand)) `shouldBe` True
            roundtrips (SomePlayerHandFSM PHDecisionFSM) `shouldBe` True
            roundtrips (SomeDealerHandFSM DHAwaitingFirstCardFSM) `shouldBe` True
            roundtrips (SomePlayerFSM PPlayingHandFSM) `shouldBe` True
            roundtrips (SomePlayerFSM PDoneFSM) `shouldBe` True
            roundtrips (PeekRound (SomePeekFSM PeekAwaitingFSM)) `shouldBe` True
            roundtrips (ENHCRound (SomeENHCFSM ENHCPlayersFSM)) `shouldBe` True

    describe "Entity States" $ do
        it "Player roundtrips" $ do
            let player =
                    EPlayer
                        (PlayerAttrs "Test Player" (Chips 1000))
                        (PlayerModes (SomePlayerFSM PPlayingHandFSM))
                        (PlayerRels (Present (EntityId 600 :: RoundId)) (Present (EntityId 800 :: TableId)))
            roundtrips player `shouldBe` True

        it "BoutDealer roundtrips" $ do
            let boutDealer =
                    EBout
                        (BoutAttrs (SomeHand [] (HandWitness HardWitness EmptyWitness NoneWitness [] 0)) (characterize [Card Ten Hearts, Card Nine Spades]) Hand1 Absent)
                        (BoutModes (SomeBoutFSM BPlayerTurnFSM) (SomePlayerHandFSM PHDecisionFSM) (SomeDealerHandFSM DHEvaluatingFSM))
                        (BoutRels (EntityId 100 :: PlayerId) (EntityId 600 :: DealerId) (EntityId 700 :: RoundId) (EntityId 800 :: TableId) (singletonFiniteMap Hand1 (Present (EntityId 300 :: BoutId)) Absent))
            roundtrips boutDealer `shouldBe` True

        it "BoutPlayer roundtrips" $ do
            let boutPlayer =
                    EBout
                        (BoutAttrs (SomeHand [] (HandWitness HardWitness EmptyWitness NoneWitness [] 0)) (SomeHand [] (HandWitness HardWitness EmptyWitness NoneWitness [] 0)) Hand1 Absent)
                        (BoutModes (SomeBoutFSM BPlayerTurnFSM) (SomePlayerHandFSM PHDecisionFSM) (SomeDealerHandFSM DHEvaluatingFSM))
                        (BoutRels (EntityId 100 :: PlayerId) (EntityId 200 :: DealerId) (EntityId 700 :: RoundId) (EntityId 800 :: TableId) (singletonFiniteMap Hand1 (Present (EntityId 300 :: BoutId)) Absent))
            roundtrips boutPlayer `shouldBe` True

        it "Bout roundtrips" $ do
            let bout =
                    EBout
                        (BoutAttrs (SomeHand [] (HandWitness HardWitness EmptyWitness NoneWitness [] 0)) (SomeHand [] (HandWitness HardWitness EmptyWitness NoneWitness [] 0)) Hand1 (Present pushOutcome))
                        (BoutModes (SomeBoutFSM BPlayerTurnFSM) (SomePlayerHandFSM PHDecisionFSM) (SomeDealerHandFSM DHEvaluatingFSM))
                        (BoutRels (EntityId 100 :: PlayerId) (EntityId 200 :: DealerId) (EntityId 800 :: RoundId) (EntityId 300 :: TableId) (singletonFiniteMap Hand1 (Present (EntityId 400 :: BoutId)) Absent))
            roundtrips bout `shouldBe` True

    describe "Delta Types" $ do
        it "Entity deltas roundtrip" $ do
            let history = CausalHistory (Just (IntentId 123)) (Just (EventId 456))
            let boutPlayerHandDelta =
                    AttrsDelta
                        history
                        ( DBoutSetPlayerHand
                            (characterize [Card King Hearts])
                            (characterize [Card Ten Hearts])
                        ) ::
                        SomeDelta 'Bout
            let boutPlayerHandFSMDelta =
                    ModesDelta
                        history
                        ( DBoutSetPlayerHandFSM
                            (SomePlayerHandFSM PHHittingFSM)
                            (SomePlayerHandFSM PHDecisionFSM)
                        ) ::
                        SomeDelta 'Bout
            let boutPlayerDelta =
                    ModesDelta
                        history
                        ( DBoutSetBoutFSM
                            (SomeBoutFSM BPlayerTurnFSM)
                            (SomeBoutFSM BAwaitingFirstCardFSM)
                        ) ::
                        SomeDelta 'Bout
            let dealerDelta = AttrsDelta history (DDealerSetName "New" "Old") :: SomeDelta 'Dealer
            let tableDelta = AttrsDelta history (DTableSetName "Table2" "Table1") :: SomeDelta 'Table
            roundtrips boutPlayerHandDelta `shouldBe` True
            roundtrips boutPlayerHandFSMDelta `shouldBe` True
            roundtrips boutPlayerDelta `shouldBe` True
            roundtrips dealerDelta `shouldBe` True
            roundtrips tableDelta `shouldBe` True

    describe "Event Types" $ do
        it "BlackjackEvent roundtrips" $ do
            let events =
                    [ BoutPlayerCardDealt (Card Ace Hearts) (EntityId 400 :: BoutId)
                    , BoutPlayerStood (EntityId 100 :: BoutId)
                    , BoutPlayerHit (EntityId 100 :: BoutId)
                    , BoutPlayerDoubledDown (EntityId 100 :: BoutId)
                    , BoutPlayerSplit (EntityId 100 :: BoutId)
                    , BoutPlayerSurrendered (EntityId 100 :: BoutId)
                    , BoutSettled (EntityId 300 :: BoutId) dealerWinsHigher
                    , BoutDealerRevealed (EntityId 500 :: BoutId)
                    ]
            forM_ events $ \event ->
                roundtrips event `shouldBe` True

    describe "Agent Types" $ do
        it "PlayerArchetype roundtrips" $ do
            chart <- loadCanonicalStrategy
            let archetypes =
                    [ SomePlayerBasicStrategy
                        ( BasicStrategyArchetype
                            (BasicConfig chart (MistakeProfile 0.05 workingMistakeDistribution))
                            (BasicState 10 emptySessionStats)
                        )
                    , SomePlayerPerfect
                        ( PerfectArchetype
                            (PerfectConfig True)
                            (PerfectState 5 emptySessionStats)
                        )
                    , SomePlayerAdvantage
                        ( AdvantageArchetype
                            (AdvantageConfig HiLo FlatBetting (DeviationChart Map.empty))
                            (AdvantageState 0 0.0 6.0 emptySessionStats)
                        )
                    , SomePlayerSuperstitious
                        ( SuperstitiousArchetype
                            (SuperstitionConfig [NeverHitOn16, AlwaysSplitAces] chart)
                            (SuperstitionState 20 3 emptySessionStats)
                        )
                    ]
            forM_ archetypes $ \archetype ->
                roundtrips archetype `shouldBe` True

    describe "Game Rules" $ do
        it "GameRuleSet roundtrips" $ do
            let rulesets =
                    [ mkS17Rules
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

    describe "Strategy Charts" $ do
        it "ChartEntry serializes" $ do
            let entry =
                    ChartEntry
                        HardHand
                        (Just 16)
                        (Map.fromList [(Two, MoveHit), (Ten, MoveSurrenderOrStand)])
            case decode (encode entry) :: Maybe ChartEntry of
                Just _ -> True `shouldBe` True
                Nothing -> False `shouldBe` True

    describe "Collections" $ do
        it "Registry roundtrips" $ do
            let player =
                    EPlayer
                        (PlayerAttrs "Test" (Chips 1000))
                        (PlayerModes (SomePlayerFSM PPlayingHandFSM))
                        (PlayerRels Absent Absent)
            let timeline = mkTimeline (EntityId 100 :: PlayerId) (Tick 1000) player
            let reg = Registry (IHM.singleton (EntityId 100 :: PlayerId) timeline) :: Registry 'Player (SomeDelta 'Player)
            roundtrips reg `shouldBe` True

        it "FiniteMap roundtrips" $ do
            let fm = singletonFiniteMap Hand1 (Present (EntityId 400 :: BoutId)) Absent
            roundtrips fm `shouldBe` True

    describe "Trace Operations" $ do
        it "TraceOp roundtrips" $ do
            let player =
                    EPlayer
                        (PlayerAttrs "Test" (Chips 1000))
                        (PlayerModes (SomePlayerFSM PPlayingHandFSM))
                        (PlayerRels Absent Absent)
            let boutPlayer =
                    EBout
                        (BoutAttrs (characterize []) (SomeHand [] (HandWitness HardWitness EmptyWitness NoneWitness [] 0)) Hand1 Absent)
                        (BoutModes (SomeBoutFSM BPlayerTurnFSM) (SomePlayerHandFSM PHDecisionFSM) (SomeDealerHandFSM DHEvaluatingFSM))
                        (BoutRels (EntityId 100 :: PlayerId) (EntityId 200 :: DealerId) (EntityId 800 :: RoundId) (EntityId 900 :: TableId) (singletonFiniteMap Hand1 (Present (EntityId 300 :: BoutId)) Absent))
            let bout =
                    EBout
                        (BoutAttrs (SomeHand [] (HandWitness HardWitness EmptyWitness NoneWitness [] 0)) (SomeHand [] (HandWitness HardWitness EmptyWitness NoneWitness [] 0)) Hand1 Absent)
                        (BoutModes (SomeBoutFSM BAwaitingFirstCardFSM) (SomePlayerHandFSM PHAwaitingFirstCardFSM) (SomeDealerHandFSM DHAwaitingFirstCardFSM))
                        (BoutRels (EntityId 100 :: PlayerId) (EntityId 200 :: DealerId) (EntityId 800 :: RoundId) (EntityId 700 :: TableId) (singletonFiniteMap Hand1 (Present (EntityId 300 :: BoutId)) Absent))
            let ops =
                    [ bear (EntityId 100 :: PlayerId) player
                    , mutate
                        PlayerWitness
                        (EntityId 100 :: PlayerId)
                        ( AttrsDelta
                            (CausalHistory Nothing Nothing)
                            (DPlayerSetBankroll (Chips 2000) (Chips 1000))
                        )
                    , bear (EntityId 700 :: BoutId) boutPlayer
                    , bear (EntityId 300 :: BoutId) bout
                    , bury BoutWitness (EntityId 300 :: BoutId) (BoutComplete pushOutcome)
                    ]
            forM_ ops $ \op ->
                roundtrips op `shouldBe` True
