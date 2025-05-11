{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerSpot.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.FSM.PlayerSpot
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerSpot

data PlayerSpotEntityModesDelta
    = ReplaceFSM SomePlayerSpotFSM SomePlayerSpotFSM
    deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotEntityModesDelta
instance FromJSON PlayerSpotEntityModesDelta

instance Incremental PlayerSpotEntityModesDelta where
    type Target PlayerSpotEntityModesDelta = PlayerSpotEntityModes

    applyDelta (ReplaceFSM _ new) state = state{_playerSpotEntityModesPlayerSpot = new}

    previewDelta delta state = Just (applyDelta delta state)

    describeDelta (ReplaceFSM _ new) _ =
        "Replaced PlayerSpot FSM with: " ++ show new

instance Reversible PlayerSpotEntityModesDelta where
    invert (ReplaceFSM old new) = Right (ReplaceFSM new old)
