{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerRound.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerRound

data DealerRoundEntityModesDelta = NoopModes
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntityModesDelta
instance FromJSON DealerRoundEntityModesDelta

instance Incremental DealerRoundEntityModesDelta where
    type Target DealerRoundEntityModesDelta = DealerRoundEntityModes

    applyDelta :: DealerRoundEntityModesDelta -> DealerRoundEntityModes -> DealerRoundEntityModes
    applyDelta NoopModes e = e

    previewDelta :: DealerRoundEntityModesDelta -> DealerRoundEntityModes -> Maybe DealerRoundEntityModes
    previewDelta NoopModes = Just

    describeDelta :: DealerRoundEntityModesDelta -> DealerRoundEntityModes -> String
    describeDelta NoopModes _ = "Noop FSM delta (DealerRound)"

instance Reversible DealerRoundEntityModesDelta where
    invert NoopModes = Right NoopModes
