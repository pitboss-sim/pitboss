{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Player.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities

data PlayerEntityModesDelta
  = NoopFSM
  deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityModesDelta

instance FromJSON PlayerEntityModesDelta

instance Incremental PlayerEntityModesDelta where
  type Entity PlayerEntityModesDelta = () -- Replace with actual FSM type when added

  applyDelta NoopFSM e = e
  previewDelta NoopFSM = Just
  describeDelta NoopFSM _ = "Noop FSM delta"

instance Reversible PlayerEntityModesDelta where
  invert NoopFSM = Right NoopFSM
