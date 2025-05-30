module Pitboss.Orchestration.EventGenSpec (spec) where

import Pitboss.Orchestration.EventGen
import Pitboss.State.Entity.Types
import Pitboss.State.Types.Core
import Test.Hspec

spec :: Spec
spec = describe "EventGen" $ do
    it "transforms intent to event entity" $ do
        let intentId = EntityId 999
            details = TableDealCardIntent (EntityId 456)
            timestamp = Tick 1000
            event = processIntent intentId details timestamp

        case event of
            EEvent attrs _ rels -> do
                _eventAttrsType attrs `shouldBe` IntentValidated
                _eventAttrsDetails attrs `shouldBe` IntentValidatedDetails intentId
                _eventRelsCausingIntent rels `shouldBe` intentId
                _eventAttrsDescription attrs
                    `shouldBe` "Table deal card intent validated for hand EntityId 456"
