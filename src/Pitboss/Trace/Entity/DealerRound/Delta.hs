{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.Trace.Entity.DealerRound.Delta (
    module Pitboss.Trace.Entity.DealerRound.Delta.Attrs,
    module Pitboss.Trace.Entity.DealerRound.Delta.Modes,
    module Pitboss.Trace.Entity.DealerRound.Delta.Rels,
    DealerRoundEntityDelta (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerRound
import Pitboss.Trace.Entity.DealerRound.Delta.Attrs
import Pitboss.Trace.Entity.DealerRound.Delta.Modes
import Pitboss.Trace.Entity.DealerRound.Delta.Rels

data DealerRoundEntityDelta
    = DealerRoundEntityAttrsDelta DealerRoundEntityAttrsDelta
    | DealerRoundEntityModesDelta DealerRoundEntityModesDelta
    | DealerRoundEntityRelsDelta DealerRoundEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntityDelta

instance FromJSON DealerRoundEntityDelta

instance Incremental DealerRoundEntityDelta where
    type Target DealerRoundEntityDelta = DealerRoundEntity

    applyDelta delta entity = case delta of
        DealerRoundEntityAttrsDelta d ->
            entity{_dealerRoundEntityAttrs = applyDelta d (_dealerRoundEntityAttrs entity)}
        DealerRoundEntityModesDelta d ->
            entity{_dealerRoundEntityModes = applyDelta d (_dealerRoundEntityModes entity)}
        DealerRoundEntityRelsDelta d ->
            entity{_dealerRoundEntityRels = applyDelta d (_dealerRoundEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        DealerRoundEntityAttrsDelta d -> describeDelta d (_dealerRoundEntityAttrs entity)
        DealerRoundEntityModesDelta d -> describeDelta d (_dealerRoundEntityModes entity)
        DealerRoundEntityRelsDelta d -> describeDelta d (_dealerRoundEntityRels entity)

instance Reversible DealerRoundEntityDelta where
    invert = \case
        DealerRoundEntityAttrsDelta d -> DealerRoundEntityAttrsDelta <$> invert d
        DealerRoundEntityModesDelta d -> DealerRoundEntityModesDelta <$> invert d
        DealerRoundEntityRelsDelta d -> DealerRoundEntityRelsDelta <$> invert d
