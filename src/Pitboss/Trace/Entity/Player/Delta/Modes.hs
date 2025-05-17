{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Player.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities

data PlayerEntityModesDelta = NoopModes
  deriving (Eq, Show, Generic)

instance Incremental PlayerEntityModesDelta where
  type Target PlayerEntityModesDelta = PlayerEntityModes
  applyDelta NoopModes e = e
  previewDelta NoopModes = Just
  describeDelta NoopModes _ = "Noop FSM delta"

instance Reversible PlayerEntityModesDelta where
  invert NoopModes = Right NoopModes

instance ToJSON PlayerEntityModesDelta

instance FromJSON PlayerEntityModesDelta
