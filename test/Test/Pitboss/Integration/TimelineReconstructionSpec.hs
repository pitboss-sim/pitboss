{-# LANGUAGE DataKinds #-}

module Test.Pitboss.Integration.TimelineReconstructionSpec where

import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Test.Hspec

spec :: Spec
spec = describe "Timeline Integration" $ do
    it "stores delta and reconstructs entity at tick" $ do
        let scenario = mkTimelineTestScenario
            cache = buildCacheWithStandDelta scenario

        case withTickCache cache $ deref (contestantId scenario) of
            Just contestant ->
                _cModesBoutFSM (_cModes contestant) `shouldBe` Present (SomeContestantBoutFSM CBResolvedFSM)
            Nothing ->
                expectationFailure "Contestant not found in cache"

    it "reconstructs entity at initial tick without deltas" $ do
        let scenario = mkTimelineTestScenario
            cache = buildInitialCache scenario

        case withTickCache cache $ deref (contestantId scenario) of
            Just contestant ->
                _cModesBoutFSM (_cModes contestant) `shouldBe` Present (SomeContestantBoutFSM CBPlayingFSM)
            Nothing ->
                expectationFailure "Contestant not found in cache"

-- Local test scenario helpers

data TimelineTestScenario = TimelineTestScenario
    { handId :: HandId
    , contestantId :: ContestantId
    , initialTick :: Tick
    , updateTick :: Tick
    , initialContestant :: EntityState 'Contestant
    , initialHand :: EntityState 'Hand
    }

mkTimelineTestScenario :: TimelineTestScenario
mkTimelineTestScenario =
    let hId = HandId 500
        cId = ContestantId 600
        initTick = Tick 1000
        updTick = Tick 1001

        hand =
            mkTestHand
                (ContestantOwner cId)
                (characterize [Card Ten Hearts, Card Nine Spades])

        contestant = mkTestContestant hId cId
     in TimelineTestScenario hId cId initTick updTick contestant hand

mkTestHand :: HandOwner -> SomeHand -> EntityState 'Hand
mkTestHand owner hand =
    EHand
        { _hAttrs = HandAttrs hand
        , _hModes = HandModes (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM))
        , _hRels = HandRels owner (BoutId 300) (ShoeId 900)
        }

mkTestContestant :: HandId -> ContestantId -> EntityState 'Contestant
mkTestContestant _ _ =
    EContestant
        { _cAttrs = ContestantAttrs Hand1
        , _cModes =
            ContestantModes
                { _cModesBoutFSM = Present (SomeContestantBoutFSM CBPlayingFSM)
                , _cModesRoundFSM = SomeContestantRoundFSM CREngagedFSM
                }
        , _cRels = ContestantRels (singletonFiniteMap Hand1 (Present (BoutId 300)) Absent) (PlayerId 100) (RoundId 700) (ShoeId 900)
        }

buildCacheWithStandDelta :: TimelineTestScenario -> TickCache
buildCacheWithStandDelta scenario =
    let contestantTimeline = mkTimeline (contestantId scenario) (initialTick scenario) (initialContestant scenario)
        standDelta =
            ModesDelta
                (CausalHistory (Just (IntentId 100)) (Just (EventId 200)))
                ( DContestantSetBoutFSM
                    (Present (SomeContestantBoutFSM CBResolvedFSM))
                    (Present (SomeContestantBoutFSM CBPlayingFSM))
                )

        updatedTimeline =
            contestantTimeline
                { timelineDeltas = IHM.insert (updateTick scenario) [standDelta] (timelineDeltas contestantTimeline)
                }

        contestantRegistry = Registry $ IHM.singleton (contestantId scenario) updatedTimeline
     in populateTickCache
            mempty
            contestantRegistry
            mempty
            mempty
            mempty
            mempty
            mempty
            mempty
            (updateTick scenario)

buildInitialCache :: TimelineTestScenario -> TickCache
buildInitialCache scenario =
    let contestantTimeline = mkTimeline (contestantId scenario) (initialTick scenario) (initialContestant scenario)
        contestantRegistry = Registry $ IHM.singleton (contestantId scenario) contestantTimeline
     in populateTickCache
            mempty
            contestantRegistry
            mempty
            mempty
            mempty
            mempty
            mempty
            mempty
            (initialTick scenario)
