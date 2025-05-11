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

  applyDelta (ReplaceFSM _ new) entity = entity {_playerSpotEntityModesPlayerSpot = new}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta _ _ = "Placeholder"

instance Reversible PlayerSpotEntityModesDelta where
  invert = \case
    ReplaceFSM old new -> Right (ReplaceFSM new old)

describePlayerSpotEntityModesDelta :: PlayerSpotEntityModesDelta -> String
describePlayerSpotEntityModesDelta _ = "Replaced PlayerSpot FSM"
