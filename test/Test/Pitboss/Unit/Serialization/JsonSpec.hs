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

roundtrips :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Bool
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
            roundtrips (SomeContestantHandFSM CHDecisionFSM) `shouldBe` True
            roundtrips (SomeContestantHandFSM (CHResolvedFSM CStand)) `shouldBe` True
            roundtrips (SomeDealerHandFSM DHDealingFSM) `shouldBe` True
            roundtrips (SomeDealerHandFSM (DHResolvedFSM DDealerStand)) `shouldBe` True
            roundtrips (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM)) `shouldBe` True
            roundtrips (DealerHandFSM (SomeDealerHandFSM DHDealingFSM)) `shouldBe` True
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
                        (PlayerRels (Present (RoundId 600)) (Present (TableId 800)))
            roundtrips player `shouldBe` True

        it "Hand roundtrips" $ do
            let hand =
                    EHand
                        (HandAttrs (characterize [Card Ten Hearts, Card Nine Spades]))
                        (HandModes (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM)))
                        (HandRels (ContestantOwner (ContestantId 600)) (BoutId 300) (ShoeId 900))
            roundtrips hand `shouldBe` True

        it "Contestant roundtrips" $ do
            let contestant =
                    EContestant
                        (ContestantAttrs Hand1)
                        ( ContestantModes
                            (Present (SomeContestantBoutFSM CBPlayingFSM))
                            (SomeContestantRoundFSM CREngagedFSM)
                        )
                        (ContestantRels (singletonFiniteMap Hand1 (Present (BoutId 300)) Absent) (PlayerId 100) (RoundId 700) (ShoeId 900))
            roundtrips contestant `shouldBe` True

        it "Bout roundtrips" $ do
            let bout =
                    EBout
                        (BoutAttrs (Present pushOutcome))
                        (BoutModes (SomeBoutFSM BPlayerTurnFSM))
                        (BoutRels (HandId 400) (HandId 900) (RoundId 800) (ShoeId 700) (TableId 300))
            roundtrips bout `shouldBe` True

    describe "Delta Types" $ do
        it "Entity deltas roundtrip" $ do
            let history = CausalHistory (Just (IntentId 123)) (Just (EventId 456))
            let handDelta =
                    AttrsDelta
                        history
                        ( DHandSetHand
                            (characterize [Card King Hearts])
                            (characterize [Card Ten Hearts])
                        ) ::
                        SomeDelta 'Hand
            let handFSMDelta =
                    ModesDelta
                        history
                        ( DHandSetHandFSM
                            (ContestantHandFSM (SomeContestantHandFSM CHHittingFSM))
                            (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM))
                        ) ::
                        SomeDelta 'Hand
            let contestantDelta =
                    ModesDelta
                        history
                        ( DContestantSetBoutFSM
                            (Present (SomeContestantBoutFSM CBPlayingFSM))
                            (Present (SomeContestantBoutFSM CBAwaitingCardsFSM))
                        ) ::
                        SomeDelta 'Contestant
            let dealerDelta = AttrsDelta history (DDealerSetName "New" "Old") :: SomeDelta 'Dealer
            let tableDelta = AttrsDelta history (DTableSetName "Table2" "Table1") :: SomeDelta 'Table
            roundtrips handDelta `shouldBe` True
            roundtrips handFSMDelta `shouldBe` True
            roundtrips contestantDelta `shouldBe` True
            roundtrips dealerDelta `shouldBe` True
            roundtrips tableDelta `shouldBe` True

    describe "Event Types" $ do
        it "BlackjackEvent roundtrips" $ do
            let events =
                    [ CardDealt (Card Ace Hearts) (HandId 400)
                    , ContestantStood (ContestantId 100) (HandId 400)
                    , ContestantHit (ContestantId 100) (HandId 400)
                    , ContestantDoubledDown (ContestantId 100) (HandId 400)
                    , ContestantSplit (ContestantId 100) (HandId 400)
                    , ContestantSurrendered (ContestantId 100) (HandId 400)
                    , BoutSettled (BoutId 300) dealerWinsHigher
                    , DealerRevealed (DealerId 200) (HandId 500)
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
            let timeline = mkTimeline (PlayerId 100) (Tick 1000) player
            let reg = Registry (IHM.singleton (PlayerId 100) timeline) :: Registry 'Player (SomeDelta 'Player)
            roundtrips reg `shouldBe` True

        it "FiniteMap roundtrips" $ do
            let fm = singletonFiniteMap Hand1 (Present (HandId 400)) Absent
            roundtrips fm `shouldBe` True

    describe "Trace Operations" $ do
        it "TraceOp roundtrips" $ do
            let player =
                    EPlayer
                        (PlayerAttrs "Test" (Chips 1000))
                        (PlayerModes (SomePlayerFSM PPlayingHandFSM))
                        (PlayerRels Absent Absent)
            let hand =
                    EHand
                        (HandAttrs (characterize []))
                        (HandModes (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM)))
                        (HandRels (ContestantOwner (ContestantId 700)) (BoutId 300) (ShoeId 900))
            let bout =
                    EBout
                        (BoutAttrs Absent)
                        (BoutModes (SomeBoutFSM BAwaitingFirstCardFSM))
                        (BoutRels (HandId 400) (HandId 900) (RoundId 800) (ShoeId 700) (TableId 800))
            let ops =
                    [ createBirth (PlayerId 100) player
                    , createMutation
                        (PlayerId 100)
                        player
                        ( AttrsDelta
                            (CausalHistory Nothing Nothing)
                            (DPlayerSetBankroll (Chips 2000) (Chips 1000))
                        )
                    , createBirth (HandId 400) hand
                    , createDeath (HandId 400) hand (HandResolved (HOContestant CBust))
                    , createBirth (BoutId 300) bout
                    , createDeath (BoutId 300) bout (BoutComplete pushOutcome)
                    ]
            forM_ ops $ \op ->
                roundtrips op `shouldBe` True
