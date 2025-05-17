{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerRound.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerRound

data DealerRoundEntityModesDelta = NoopModes
  deriving (Eq, Show, Generic)

instance Incremental DealerRoundEntityModesDelta where
  type Entity DealerRoundEntityModesDelta = DealerRoundEntityModes
  applyDelta NoopModes e = e
  previewDelta NoopModes = Just
  describeDelta NoopModes _ = "Noop FSM delta"

instance Reversible DealerRoundEntityModesDelta where
  invert NoopModes = Right NoopModes

instance ToJSON DealerRoundEntityModesDelta
