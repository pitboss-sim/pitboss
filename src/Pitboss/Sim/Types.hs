{-# LANGUAGE DataKinds #-}

module Pitboss.Sim.Types where

import Data.Word (Word64)
import Pitboss.Blackjack
import Pitboss.State.Trace.Ops
import Pitboss.State.Types.Core

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
