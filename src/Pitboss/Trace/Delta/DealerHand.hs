{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.DealerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Delta.DealerHand.FSM
import Pitboss.Trace.Delta.DealerHand.Relation
import Pitboss.Trace.Delta.DealerHand.State
import Pitboss.Trace.Entity.DealerHand

data DealerHandDelta
  = DealerHandStateDelta DealerHandStateDelta
  | DealerHandRelationsDelta DealerHandRelationsDelta
  | DealerHandFSMDelta DealerHandFSMDelta
  deriving (Eq, Show, Generic)

instance ToJSON DealerHandDelta

instance FromJSON DealerHandDelta

instance Incremental DealerHandDelta where
  type Entity DealerHandDelta = DealerHand

  applyDelta delta entity = case delta of
    DealerHandStateDelta d -> entity {_state = applyDelta d (_state entity)}
    DealerHandRelationsDelta d -> entity {_rels = applyDelta d (_rels entity)}
    DealerHandFSMDelta d -> applyDelta d entity

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    DealerHandStateDelta sd -> describeDelta sd (_state entity)
    DealerHandRelationsDelta rd -> describeDelta rd (_rels entity)
    DealerHandFSMDelta _ -> "FSM replaced"

instance Reversible DealerHandDelta where
  invert = \case
    DealerHandStateDelta d -> DealerHandStateDelta <$> invert d
    DealerHandRelationsDelta d -> DealerHandRelationsDelta <$> invert d
    DealerHandFSMDelta d -> DealerHandFSMDelta <$> invert d
