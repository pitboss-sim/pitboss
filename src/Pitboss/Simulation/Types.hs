{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict.InsOrd
import Data.Word (Word64)
import GHC.Generics (Generic)
import Pitboss.Causality
import Pitboss.Simulation.Event

data SimState = SimState
    { simTrace :: Trace
    , simEventLog :: EventLog
    , simIntentLog :: IntentLog
    , simTick :: Tick
    }

newtype EventLog = EventLog
    { eventLogEvents :: InsOrdHashMap Tick [SimEvent]
    }
    deriving (Eq, Show, Generic)

instance ToJSON EventLog
instance FromJSON EventLog

newtype IntentLog = IntentLog
    { intentLogIntents :: InsOrdHashMap Tick [SimIntent]
    }
    deriving (Eq, Show, Generic)

instance ToJSON IntentLog
instance FromJSON IntentLog

data SimIntent = SimIntent
    { intentId :: IntentId
    , intentActor :: ActorId
    , intentDesiredEvent :: BlackjackEvent
    , intentTimestamp :: Tick
    }
    deriving (Eq, Show, Generic)

instance ToJSON SimIntent
instance FromJSON SimIntent

data SimEvent = SimEvent
    { eventId :: EventId
    , eventOccurred :: BlackjackEvent
    , eventTimestamp :: Tick
    , eventCausingIntent :: Maybe Word64
    }
    deriving (Eq, Show, Generic)

instance ToJSON SimEvent
instance FromJSON SimEvent

data SimLogEntry
    = IntentLogged SimIntent
    | IntentValidated Word64 ValidationOutcome
    | EventLogged SimEvent
    | TraceOp TraceOp
    deriving (Eq, Show, Generic)

instance ToJSON SimLogEntry
instance FromJSON SimLogEntry

data ValidationOutcome = Accepted | Rejected RejectionReason
    deriving (Eq, Show, Generic)

instance ToJSON ValidationOutcome
instance FromJSON ValidationOutcome

data RejectionReason = NotYourTurn | InvalidAction | InsufficientFunds
    deriving (Eq, Show, Generic)

instance ToJSON RejectionReason
instance FromJSON RejectionReason

data ActorId
    = ActorPlayer (EntityId 'Player)
    | ActorDealer (EntityId 'Dealer)
    | ActorTable (EntityId 'Table)
    deriving (Eq, Show, Generic)

instance ToJSON ActorId
instance FromJSON ActorId
