{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Table.Delta.Modes where

import GHC.Generics
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities

data TableEntityModesDelta = NoopModes
    deriving (Eq, Show, Generic)

instance Incremental TableEntityModesDelta where
    type Target TableEntityModesDelta = TableEntityModes
    applyDelta NoopModes e = e
    previewDelta NoopModes = Just
    describeDelta NoopModes _ = "Noop FSM delta"

instance Reversible TableEntityModesDelta where
    invert NoopModes = Right NoopModes
