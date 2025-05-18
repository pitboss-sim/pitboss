{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.Trace.Entity.Offering.Delta (
    module Pitboss.Trace.Entity.Offering.Delta.Attrs,
    module Pitboss.Trace.Entity.Offering.Delta.Modes,
    module Pitboss.Trace.Entity.Offering.Delta.Rels,
    OfferingEntityDelta (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Offering.Delta.Attrs
import Pitboss.Trace.Entity.Offering.Delta.Modes
import Pitboss.Trace.Entity.Offering.Delta.Rels

data OfferingEntityDelta
    = OfferingEntityAttrsDelta OfferingEntityAttrsDelta
    | OfferingEntityRelsDelta OfferingEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON OfferingEntityDelta

instance FromJSON OfferingEntityDelta

instance Incremental OfferingEntityDelta where
    type Target OfferingEntityDelta = OfferingEntity

    applyDelta delta entity = case delta of
        OfferingEntityAttrsDelta d -> entity{_offeringEntityAttrs = applyDelta d (_offeringEntityAttrs entity)}
        OfferingEntityRelsDelta d -> entity{_offeringEntityRels = applyDelta d (_offeringEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        OfferingEntityAttrsDelta d -> describeDelta d (_offeringEntityAttrs entity)
        OfferingEntityRelsDelta d -> describeDelta d (_offeringEntityRels entity)

instance Reversible OfferingEntityDelta where
    invert = \case
        OfferingEntityAttrsDelta d -> OfferingEntityAttrsDelta <$> invert d
        OfferingEntityRelsDelta d -> OfferingEntityRelsDelta <$> invert d
