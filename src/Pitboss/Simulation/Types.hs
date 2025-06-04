{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Simulation.Types where

import Data.Aeson.Types
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
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype EventLog = EventLog
    { eventLogEvents :: InsOrdHashMap Tick [SimEvent]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype IntentLog = IntentLog
    { intentLogIntents :: InsOrdHashMap Tick [SimIntent]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SimIntent = SimIntent
    { intentId :: IntentId
    , intentActor :: ActorId
    , intentDesiredEvent :: SimulationEvent
    , intentTimestamp :: Tick
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SimEvent = SimEvent
    { eventId :: EventId
    , eventOccurred :: SimulationEvent
    , eventTimestamp :: Tick
    , eventCausingIntent :: Maybe Word64
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SimLogEntry
    = IntentLogged SimIntent
    | IntentValidated Word64 ValidationOutcome
    | EventLogged SimEvent
    | TraceOp TraceOp
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ValidationOutcome = Accepted | Rejected RejectionReason
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data RejectionReason = NotYourTurn | InvalidAction | InsufficientFunds
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ActorId
    = ActorPlayer PlayerId
    | ActorDealer DealerId
    | ActorTable TableId
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
