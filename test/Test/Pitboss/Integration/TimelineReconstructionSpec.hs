{-# LANGUAGE DataKinds #-}

module Test.Pitboss.Integration.TimelineReconstructionSpec where

import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Test.Hspec

spec :: Spec
spec = describe "Timeline Integration" $ do
    it "stores delta and reconstructs BoutPlayer entity at tick" $ do
        let scenario = mkTimelineTestScenario
            cache = buildCacheWithStandDelta scenario

        case withTickCache cache $ deref (boutId scenario) of
            Just bout' ->
                _bModesBoutFSM (_bModes bout') `shouldBe` SomeBoutFSM BDoneFSM
            Nothing ->
                expectationFailure "Bout not found in cache"

    it "reconstructs Bout entity at initial tick without deltas" $ do
        let scenario = mkTimelineTestScenario
            cache = buildInitialCache scenario

        case withTickCache cache $ deref (boutId scenario) of
            Just bout' ->
                _bModesBoutFSM (_bModes bout') `shouldBe` SomeBoutFSM BPlayerTurnFSM
            Nothing ->
                expectationFailure "Bout not found in cache"

data TimelineTestScenario = TimelineTestScenario
    { boutId :: BoutId
    , initialTick :: Tick
    , updateTick :: Tick
    , initialBout :: EntityState 'Bout
    }

mkTimelineTestScenario :: TimelineTestScenario
mkTimelineTestScenario =
    let cId = EntityId 600 :: BoutId
        initTick = Tick 1000
        updTick = Tick 1001
        bout' = mkTestBoutForTimeline cId
     in TimelineTestScenario cId initTick updTick bout'

mkTestBoutForTimeline :: BoutId -> EntityState 'Bout
mkTestBoutForTimeline _boutId =
    EBout
        { _bAttrs = BoutAttrs (characterize [Card Ten Hearts, Card Nine Spades]) (characterize [Card Ace Spades]) Hand1 Absent
        , _bModes =
            BoutModes
                { _bModesBoutFSM = SomeBoutFSM BPlayerTurnFSM
                , _bModesPlayerHandFSM = SomePlayerHandFSM PHDecisionFSM
                , _bModesDealerHandFSM = SomeDealerHandFSM DHAwaitingFirstCardFSM
                }
        , _bRels = BoutRels (EntityId 100 :: PlayerId) (EntityId 200 :: DealerId) (EntityId 700 :: RoundId) (EntityId 800 :: TableId) (singletonFiniteMap Hand1 (Present (EntityId 300 :: BoutId)) Absent)
        }

buildCacheWithStandDelta :: TimelineTestScenario -> TickCache
buildCacheWithStandDelta scenario =
    let boutTimeline = mkTimeline (boutId scenario) (initialTick scenario) (initialBout scenario)
        standDelta =
            ModesDelta
                (CausalHistory (Just (IntentId 100)) (Just (EventId 200)))
                ( DBoutSetBoutFSM
                    (SomeBoutFSM BDoneFSM)
                    (SomeBoutFSM BPlayerTurnFSM)
                )

        updatedTimeline =
            boutTimeline
                { timelineDeltas = IHM.insert (updateTick scenario) [standDelta] (timelineDeltas boutTimeline)
                }

        boutRegistry = Registry $ IHM.singleton (boutId scenario) updatedTimeline
     in populateTickCache
            boutRegistry
            mempty
            mempty
            mempty
            mempty
            mempty
            (updateTick scenario)

buildInitialCache :: TimelineTestScenario -> TickCache
buildInitialCache scenario =
    let boutTimeline = mkTimeline (boutId scenario) (initialTick scenario) (initialBout scenario)
        boutRegistry = Registry $ IHM.singleton (boutId scenario) boutTimeline
     in populateTickCache
            boutRegistry
            mempty
            mempty
            mempty
            mempty
            mempty
            (initialTick scenario)
