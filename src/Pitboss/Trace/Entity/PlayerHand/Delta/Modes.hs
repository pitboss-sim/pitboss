{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerHand.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.FSM.PlayerHand
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerHand

data PlayerHandEntityModesDelta
  = ReplaceFSM SomePlayerHandFSM SomePlayerHandFSM
  deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntityModesDelta

instance FromJSON PlayerHandEntityModesDelta

instance Incremental PlayerHandEntityModesDelta where
  type Target PlayerHandEntityModesDelta = PlayerHandEntityModes

  applyDelta (ReplaceFSM _ new) entity = entity {_playerHandEntityFsm = new}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta (ReplaceFSM _ new) _ = "FSM replaced:" ++ show new

instance Reversible PlayerHandEntityModesDelta where
  invert (ReplaceFSM old new) = Right (ReplaceFSM new old)
