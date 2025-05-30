{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Orchestration.EventGen where

import Pitboss.State.Entity.Types
import Pitboss.State.Types.Core

-- Transform validated intent into event entity
processIntent :: EntityId 'Intent -> IntentDetails -> Tick -> EntityState 'Event
processIntent intentId details timestamp =
    EEvent
        { _eventAttrs =
            EventAttrs
                { _eventAttrsType = IntentValidated
                , _eventAttrsDetails = IntentValidatedDetails intentId
                , _eventAttrsTimestamp = timestamp
                , _eventAttrsDescription = describeIntent details
                }
        , _eventModes = EventModes -- No modes for events currently
        , _eventRels =
            EventRels
                { _eventRelsCausingIntent = intentId
                }
        }

describeIntent :: IntentDetails -> String
describeIntent = \case
    PlayerHitIntent -> "Player hit intent validated"
    PlayerStandIntent -> "Player stand intent validated"
    PlayerDoubleIntent -> "Player double intent validated"
    PlayerSplitIntent -> "Player split intent validated"
    PlayerSurrenderIntent -> "Player surrender intent validated"
    DealerHitIntent -> "Dealer hit intent validated"
    DealerStandIntent -> "Dealer stand intent validated"
    TableDealCardIntent handId -> "Table deal card intent validated for hand " ++ show handId
    TableSettleBoutIntent boutId -> "Table settle bout intent validated for bout " ++ show boutId
