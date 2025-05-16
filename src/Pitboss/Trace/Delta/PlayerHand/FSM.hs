{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.PlayerHand.FSM where

import Data.Aeson
import GHC.Generics
import Pitboss.FSM.PlayerHandFSM
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.PlayerHand

data PlayerHandFSMDelta
  = ReplaceFSM SomePlayerHandFSM SomePlayerHandFSM
  deriving (Eq, Show, Generic)

instance ToJSON PlayerHandFSMDelta

instance FromJSON PlayerHandFSMDelta

instance Incremental PlayerHandFSMDelta where
  type Entity PlayerHandFSMDelta = PlayerHand

  applyDelta (ReplaceFSM _ new) entity = entity {_fsm = new}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta (ReplaceFSM _ new) _ = "FSM replaced:" ++ show new

instance Reversible PlayerHandFSMDelta where
  invert (ReplaceFSM old new) = Right (ReplaceFSM new old)
