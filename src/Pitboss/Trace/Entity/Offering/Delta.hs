{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.Trace.Entity.Offering.Delta
  ( module Pitboss.Trace.Entity.Offering.Delta.Attrs,
    module Pitboss.Trace.Entity.Offering.Delta.Modes,
    module Pitboss.Trace.Entity.Offering.Delta.Rels,
    OfferingDelta (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Offering
import Pitboss.Trace.Entity.Offering.Delta.Attrs
import Pitboss.Trace.Entity.Offering.Delta.Modes
import Pitboss.Trace.Entity.Offering.Delta.Rels

data OfferingDelta
  = OfferingStateDelta OfferingStateDelta
  | OfferingRelationsDelta OfferingRelationsDelta
  deriving (Eq, Show, Generic)

instance ToJSON OfferingDelta

instance FromJSON OfferingDelta

instance Incremental OfferingDelta where
  type Entity OfferingDelta = Offering

  applyDelta delta entity = case delta of
    OfferingStateDelta d -> entity {_offeringState = applyDelta d (_offeringState entity)}
    OfferingRelationsDelta d -> entity {_offeringRels = applyDelta d (_offeringRels entity)}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    OfferingStateDelta sd -> describeDelta sd (_offeringState entity)
    OfferingRelationsDelta rd -> describeDelta rd (_offeringRels entity)

instance Reversible OfferingDelta where
  invert = \case
    OfferingStateDelta d -> OfferingStateDelta <$> invert d
    OfferingRelationsDelta d -> OfferingRelationsDelta <$> invert d
