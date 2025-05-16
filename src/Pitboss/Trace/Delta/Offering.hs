{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.Offering where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Delta.Offering.Relation
import Pitboss.Trace.Delta.Offering.State
import Pitboss.Trace.Entity.Offering

data OfferingDelta
  = OfferingStateDelta OfferingStateDelta
  | OfferingRelationsDelta OfferingRelationsDelta
  deriving (Eq, Show, Generic)

instance ToJSON OfferingDelta

instance FromJSON OfferingDelta

instance Incremental OfferingDelta where
  type Entity OfferingDelta = Offering

  applyDelta delta entity = case delta of
    OfferingStateDelta d -> entity {_state = applyDelta d (_state entity)}
    OfferingRelationsDelta d -> entity {_rels = applyDelta d (_rels entity)}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    OfferingStateDelta sd -> describeDelta sd (_state entity)
    OfferingRelationsDelta rd -> describeDelta rd (_rels entity)

instance Reversible OfferingDelta where
  invert = \case
    OfferingStateDelta d -> OfferingStateDelta <$> invert d
    OfferingRelationsDelta d -> OfferingRelationsDelta <$> invert d
