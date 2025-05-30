module Pitboss.State.Sim where

-- for SimEvent, SimIntent
import Data.HashMap.Strict.InsOrd
import Pitboss.Sim.Types
import Pitboss.State.Trace
import Pitboss.State.Types.Core

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
