{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Orchestration.EventGen where

import Pitboss.Agency.Intent.Types (IntentKind (..))
import Pitboss.State.Entity.Types
import Pitboss.State.Types.Core

-- Transform validated intent into event entity
processIntent :: EntityId 'Intent -> IntentKind -> Tick -> EntityState 'Event
processIntent intentId kind timestamp =
    EEvent
        { _eventAttrs =
            EventAttrs
                { _eventAttrsType = IntentValidated
                , _eventAttrsDetails = IntentValidatedDetails intentId
                , _eventAttrsTimestamp = timestamp
                , _eventAttrsDescription = describeIntent kind
                }
        , _eventModes = EventModes -- No modes for events currently
        , _eventRels =
            EventRels
                { _eventRelsCausingIntent = intentId
                }
        }

describeIntent :: IntentKind -> String
describeIntent = \case
    IPlayerHit -> "Player hit intent validated"
    IPlayerStand -> "Player stand intent validated"
    IPlayerDouble -> "Player double intent validated"
    IPlayerSplit -> "Player split intent validated"
    IPlayerSurrender -> "Player surrender intent validated"
    IDealerHit -> "Dealer hit intent validated"
    IDealerStand -> "Dealer stand intent validated"
    IDealerDeal -> "Table deal card intent validated"
    IDealerSettleBout -> "Table settle bout intent validated"
    IDealerSettleInsurance -> "Table settle insurance intent validated"
