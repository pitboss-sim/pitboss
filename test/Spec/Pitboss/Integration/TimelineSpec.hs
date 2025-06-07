{-# LANGUAGE DataKinds #-}

module Spec.Pitboss.Integration.TimelineSpec where

import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Blackjack
import Pitboss.Causality
import Pitboss.FSM
import Test.Hspec

spec :: Spec
spec = describe "Timeline Integration" $ do
    it "stores delta and reconstructs entity at tick" $ do
        let handId = EntityId 500
            initialTick = Tick 1000
            updateTick = Tick 1001

            initialHand =
                EPlayerHand
                    { _phAttrs =
                        PlayerHandAttrs
                            { _phAttrsHand = characterize [Card Ten Hearts, Card Nine Spades]
                            , _phAttrsOriginalBet = Chips 100
                            , _phAttrsSplitDepth = 0
                            , _phAttrsHandIx = 0
                            }
                    , _phModes =
                        PlayerHandModes
                            { _phFsm = SomePlayerHandFSM PHDecisionFSM
                            }
                    , _phRels =
                        PlayerHandRels
                            { _phRelsBelongsToPlayerSpot = EntityId 600
                            , _phRelsBelongsToDealerRound = EntityId 700
                            , _phRelsOwnedByPlayer = EntityId 400
                            , _phRelsBelongsToBout = EntityId 8001
                            }
                    }

            timeline = mkTimeline handId initialTick initialHand
            -- TODO: Need to add initial creation deltas to timeline

            standDelta =
                ModesDelta
                    (CausalHistory (Just (IntentId 100)) (Just (EventId 200)))
                    ( DPlayerHandSetPlayerHandFSM
                        (SomePlayerHandFSM (PHResolvedFSM PHStand))
                        (SomePlayerHandFSM PHDecisionFSM)
                    )

            updatedTimeline =
                timeline
                    { timelineDeltas = IHM.insert updateTick [standDelta] (timelineDeltas timeline)
                    }

            registry = Registry $ IHM.singleton handId updatedTimeline

            cache =
                populateTickCache
                    mempty -- empty bout registry
                    mempty -- empty player registry
                    registry -- our hand registry with the delta
                    mempty -- empty spot registry
                    mempty -- empty dealer registry
                    mempty -- empty dealer hand registry
                    mempty -- empty dealer round registry
                    mempty -- empty table registry
                    mempty -- empty shoe registry
                    updateTick

            result = withTickCache cache $ deref (handId :: EntityId 'PlayerHand)

        case result of
            Just (EPlayerHand _ modes _) ->
                _phFsm modes `shouldBe` SomePlayerHandFSM (PHResolvedFSM PHStand)
            Nothing ->
                expectationFailure "Hand not found in cache"
