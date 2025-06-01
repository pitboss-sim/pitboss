module Pitboss.Orchestration.EventGenSpec (spec) where

import Pitboss.Agency.Intent.Types (IntentKind (..))
import Pitboss.Orchestration.EventGen
import Pitboss.State.Entity.Types
import Pitboss.State.Types.Core
import Test.Hspec

spec :: Spec
spec = describe "EventGen" $ do
    it "transforms intent to event entity" $ do
        let intentId = EntityId 999
            kind = IDealerDeal
            timestamp = Tick 1000
            event = processIntent intentId kind timestamp

        case event of
            EEvent attrs _ rels -> do
                _eventAttrsType attrs `shouldBe` IntentValidated
                _eventAttrsDetails attrs `shouldBe` IntentValidatedDetails intentId
                _eventRelsCausingIntent rels `shouldBe` intentId
                _eventAttrsDescription attrs
                    `shouldBe` "Table deal card intent validated"
