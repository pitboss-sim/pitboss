{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.PlayerSpot.FSM where

import Data.Aeson
import GHC.Generics
import Pitboss.FSM.PlayerSpotFSM
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.PlayerSpot

data PlayerSpotFSMDelta
  = ReplaceFSM SomePlayerSpotFSM SomePlayerSpotFSM
  deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotFSMDelta

instance FromJSON PlayerSpotFSMDelta

instance Incremental PlayerSpotFSMDelta where
  type Entity PlayerSpotFSMDelta = PlayerSpot

  applyDelta (ReplaceFSM _ new) entity = entity {_fsm = new}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta _ _ = "Placeholder"

instance Reversible PlayerSpotFSMDelta where
  invert = \case
    ReplaceFSM old new -> Right (ReplaceFSM new old)

describePlayerSpotFSMDelta :: PlayerSpotFSMDelta -> String
describePlayerSpotFSMDelta _ = "Replaced PlayerSpot FSM"
