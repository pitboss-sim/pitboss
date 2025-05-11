{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerHand.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.FSM.DealerHand
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerHand

data DealerHandFSMDelta
  = ReplaceFSM SomeDealerHandFSM SomeDealerHandFSM
  deriving (Eq, Show, Generic)

instance Incremental DealerHandFSMDelta where
  type Entity DealerHandFSMDelta = DealerHand

  applyDelta (ReplaceFSM _ new) entity = entity {_dealerHandFsm = new}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta _ new = "FSM replaced" ++ show new

instance ToJSON DealerHandFSMDelta

instance FromJSON DealerHandFSMDelta

instance Reversible DealerHandFSMDelta where
  invert (ReplaceFSM old new) = Right (ReplaceFSM new old)
