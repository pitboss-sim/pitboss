{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Offering.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Offering.Types

data OfferingEntityModesDelta = NoopModes
    deriving (Eq, Show, Generic)

instance ToJSON OfferingEntityModesDelta
instance FromJSON OfferingEntityModesDelta

instance Incremental OfferingEntityModesDelta where
    type Target OfferingEntityModesDelta = OfferingEntityModes

    applyDelta :: OfferingEntityModesDelta -> OfferingEntityModes -> OfferingEntityModes
    applyDelta NoopModes e = e

    previewDelta :: OfferingEntityModesDelta -> OfferingEntityModes -> Maybe OfferingEntityModes
    previewDelta NoopModes = Just

    describeDelta :: OfferingEntityModesDelta -> OfferingEntityModes -> String
    describeDelta NoopModes _ = "Noop FSM delta (Offering)"

instance Reversible OfferingEntityModesDelta where
    invert NoopModes = Right NoopModes
