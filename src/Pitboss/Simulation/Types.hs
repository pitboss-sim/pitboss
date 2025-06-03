{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Types where

import Data.HashMap.Strict.InsOrd
import Data.Word (Word64)
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

newtype IntentLog = IntentLog
    { intentLogIntents :: InsOrdHashMap Tick [SimIntent]
    }

data SimIntent = SimIntent
    { intentId :: Word64
    , intentActor :: ActorId
    , intentDesiredEvent :: BlackjackEvent
    , intentTimestamp :: Tick
    }

deriving instance Show SimIntent

data SimEvent = SimEvent
    { eventId :: Word64
    , eventOccurred :: BlackjackEvent
    , eventTimestamp :: Tick
    , eventCausingIntent :: Maybe Word64
    }

deriving instance Show SimEvent

data SimLogEntry
    = IntentLogged SimIntent
    | IntentValidated Word64 ValidationOutcome
    | EventLogged SimEvent
    | TraceOp TraceOp

deriving instance Show SimLogEntry

data ValidationOutcome = Accepted | Rejected RejectionReason
    deriving (Show)

data RejectionReason = NotYourTurn | InvalidAction | InsufficientFunds
    deriving (Show)

data ActorId
    = ActorPlayer (EntityId 'Player)
    | ActorDealer (EntityId 'Dealer)
    | ActorTable (EntityId 'Table)
    deriving (Show)
