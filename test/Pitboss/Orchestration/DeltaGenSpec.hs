module Pitboss.Orchestration.DeltaGenSpec (spec) where

import Pitboss.Agency.Intent.Types (IntentKind (..))
import Pitboss.FSM.PlayerHand
import Pitboss.Orchestration.DeltaGen
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Types.Core
import Test.Hspec

spec :: Spec
spec = describe "DeltaGen" $ do
    it "generates deltas for player stand event" $ do
        let intentId = EntityId 100
            boutId = EntityId 300
            playerId = EntityId 400

            -- The intent entity
            intent =
                EIntent
                    { _intentAttrs =
                        IntentAttrs
                            { _intentAttrsType = PlayerIntent
                            , _intentAttrsKind = IPlayerStand
                            , _intentAttrsTimestamp = Tick 999
                            , _intentAttrsDescription = "Player stands"
                            }
                    , _intentModes = IntentModes
                    , _intentRels =
                        IntentRels
                            { _intentRelsOriginatingEntity = FromPlayer playerId
                            , _intentRelsTargetBout = Just boutId
                            }
                    }

            -- The event entity
            event =
                EEvent
                    { _eventAttrs =
                        EventAttrs
                            { _eventAttrsType = IntentValidated
                            , _eventAttrsDetails = IntentValidatedDetails intentId
                            , _eventAttrsTimestamp = Tick 1000
                            , _eventAttrsDescription = "Player stand intent validated"
                            }
                    , _eventModes = EventModes
                    , _eventRels = EventRels{_eventRelsCausingIntent = intentId}
                    }

            causalHistory =
                CausalHistory
                    { causalIntent = Just intentId
                    , causalEvent = Just (EntityId 200)
                    }

            deltas = processEvent event intent causalHistory

        length deltas `shouldBe` 1
        case deltas of
            [ModesDelta hist (DPlayerHandSetPlayerHandFSM _ newFSM)] -> do
                causalIntent hist `shouldBe` Just intentId
                newFSM `shouldBe` SomePlayerHandFSM (ResolvedFSM Stand)
            _ -> expectationFailure "Expected PlayerHand FSM mode delta"
