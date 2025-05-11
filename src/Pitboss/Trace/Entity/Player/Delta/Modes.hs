{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Player.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities

data PlayerEntityModesDelta = NoopModes
    deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityModesDelta
instance FromJSON PlayerEntityModesDelta

instance Incremental PlayerEntityModesDelta where
    type Target PlayerEntityModesDelta = PlayerEntityModes

    applyDelta :: PlayerEntityModesDelta -> PlayerEntityModes -> PlayerEntityModes
    applyDelta NoopModes m = m

    previewDelta :: PlayerEntityModesDelta -> PlayerEntityModes -> Maybe PlayerEntityModes
    previewDelta NoopModes = Just

    describeDelta :: PlayerEntityModesDelta -> PlayerEntityModes -> String
    describeDelta NoopModes _ = "Noop FSM delta (Player)"

instance Reversible PlayerEntityModesDelta where
    invert NoopModes = Right NoopModes
