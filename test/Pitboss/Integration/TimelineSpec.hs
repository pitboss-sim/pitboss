{-# LANGUAGE DataKinds #-}

module Pitboss.Integration.TimelineSpec (spec) where

import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Blackjack.Materia.Card
import Pitboss.Blackjack.Materia.Chips
import Pitboss.Blackjack.Materia.Hand
import Pitboss.FSM.PlayerHand
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Registry
import Pitboss.State.TickCache
import Pitboss.State.Timeline
import Pitboss.State.Types.Core
import Test.Hspec

spec :: Spec
spec = describe "Timeline Integration" $ do
    it "stores delta and reconstructs entity at tick" $ do
        let handId = EntityId 500
            initialTick = Tick 1000
            updateTick = Tick 1001

            -- Initial hand entity
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
                            { _phFsm = SomePlayerHandFSM DecisionFSM
                            }
                    , _phRels =
                        PlayerHandRels
                            { _phRelsBelongsToPlayerSpot = EntityId 600
                            , _phRelsBelongsToDealerRound = EntityId 700
                            , _phRelsOwnedByPlayer = EntityId 400
                            }
                    }

            -- Create timeline with initial state
            timeline = mkTimeline handId initialTick initialHand
            -- TODO: Need to add initial creation deltas to timeline

            -- Create the stand delta
            standDelta =
                ModesDelta
                    (CausalHistory (Just (EntityId 100)) (Just (EntityId 200)))
                    ( DPlayerHandSetPlayerHandFSM
                        (SomePlayerHandFSM (ResolvedFSM Stand))
                        (SomePlayerHandFSM DecisionFSM)
                    )

            -- Add delta to timeline at updateTick
            updatedTimeline =
                timeline
                    { timelineDeltas = IHM.insert updateTick [standDelta] (timelineDeltas timeline)
                    }

            -- Create registry with our timeline
            registry = Registry $ IHM.singleton handId updatedTimeline

            -- Create TickCache at updateTick
            cache =
                populateTickCache
                    mempty -- empty bout registry
                    mempty -- empty player registry
                    registry -- our hand registry with the delta
                    mempty -- empty spot registry
                    mempty -- empty dealer registry
                    mempty -- empty dealer hand registry
                    mempty -- empty dealer round registry
                    mempty -- empty offering registry
                    mempty -- empty table registry
                    mempty -- empty shoe registry
                    updateTick

            -- Look up the hand in cache with explicit type
            result = withTickCache cache $ deref (handId :: EntityId 'PlayerHand)

        -- Verify hand was reconstructed with stand resolution
        case result of
            Just (EPlayerHand _ modes _) ->
                _phFsm modes `shouldBe` SomePlayerHandFSM (ResolvedFSM Stand)
            Nothing ->
                expectationFailure "Hand not found in cache"
