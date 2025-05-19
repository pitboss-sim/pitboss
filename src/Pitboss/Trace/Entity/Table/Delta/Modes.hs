{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Table.Delta.Modes where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Table.Types

data TableEntityModesDelta = NoopModes
    deriving (Eq, Show, Generic)

instance ToJSON TableEntityModesDelta

instance FromJSON TableEntityModesDelta

instance Incremental TableEntityModesDelta where
    type Target TableEntityModesDelta = TableEntityModes
    applyDelta NoopModes e = e
    previewDelta NoopModes = Just
    describeDelta NoopModes _ = "Noop FSM delta"

instance Reversible TableEntityModesDelta where
    invert NoopModes = Right NoopModes
