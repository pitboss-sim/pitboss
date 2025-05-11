{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Offering.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities

data OfferingEntityModesDelta = NoopModes
  deriving (Eq, Show, Generic)

instance Incremental OfferingEntityModesDelta where
  type Target OfferingEntityModesDelta = OfferingEntityModes
  applyDelta NoopModes e = e
  previewDelta NoopModes = Just
  describeDelta NoopModes _ = "Noop FSM delta"

instance Reversible OfferingEntityModesDelta where
  invert NoopModes = Right NoopModes

instance ToJSON OfferingEntityModesDelta
