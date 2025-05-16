{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.DealerHand.FSM where

import Data.Aeson
import GHC.Generics
import Pitboss.FSM.DealerHandFSM
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.DealerHand

data DealerHandFSMDelta
  = ReplaceFSM SomeDealerHandFSM SomeDealerHandFSM
  deriving (Eq, Show, Generic)

instance Incremental DealerHandFSMDelta where
  type Entity DealerHandFSMDelta = DealerHand

  applyDelta (ReplaceFSM _ new) entity = entity {_fsm = new}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta _ new = "FSM replaced" ++ show new

instance ToJSON DealerHandFSMDelta

instance FromJSON DealerHandFSMDelta

instance Reversible DealerHandFSMDelta where
  invert (ReplaceFSM old new) = Right (ReplaceFSM new old)
