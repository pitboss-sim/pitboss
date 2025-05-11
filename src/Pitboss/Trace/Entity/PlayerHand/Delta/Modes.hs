{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerHand.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.FSM.PlayerHand
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerHand

data PlayerHandFSMDelta
  = ReplaceFSM SomePlayerHandFSM SomePlayerHandFSM
  deriving (Eq, Show, Generic)

instance ToJSON PlayerHandFSMDelta

instance FromJSON PlayerHandFSMDelta

instance Incremental PlayerHandFSMDelta where
  type Entity PlayerHandFSMDelta = PlayerHand

  applyDelta (ReplaceFSM _ new) entity = entity {_playerHandFsm = new}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta (ReplaceFSM _ new) _ = "FSM replaced:" ++ show new

instance Reversible PlayerHandFSMDelta where
  invert (ReplaceFSM old new) = Right (ReplaceFSM new old)
