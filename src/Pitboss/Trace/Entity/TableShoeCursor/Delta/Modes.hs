{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.TableShoeCursor.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.TableShoeCursor.Types

data TableShoeCursorEntityModesDelta = NoopModes
    deriving (Eq, Show, Generic)

instance Incremental TableShoeCursorEntityModesDelta where
    type Target TableShoeCursorEntityModesDelta = TableShoeCursorEntityModes
    applyDelta NoopModes e = e
    previewDelta NoopModes = Just
    describeDelta NoopModes _ = "Noop FSM delta"

instance Reversible TableShoeCursorEntityModesDelta where
    invert NoopModes = Right NoopModes

instance ToJSON TableShoeCursorEntityModesDelta
instance FromJSON TableShoeCursorEntityModesDelta
