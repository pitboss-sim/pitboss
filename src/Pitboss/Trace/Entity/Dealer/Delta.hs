{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Dealer.Delta (
    module Pitboss.Trace.Entity.Dealer.Delta.Attrs,
    module Pitboss.Trace.Entity.Dealer.Delta.Modes,
    module Pitboss.Trace.Entity.Dealer.Delta.Rels,
    DealerEntityDelta (..),
)
where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Dealer
import Pitboss.Trace.Entity.Dealer.Delta.Attrs
import Pitboss.Trace.Entity.Dealer.Delta.Modes
import Pitboss.Trace.Entity.Dealer.Delta.Rels

data DealerEntityDelta
    = DealerEntityAttrsDelta DealerEntityAttrsDelta
    | DealerEntityRelsDelta DealerEntityRelsDelta
    | DealerEntityModesDelta DealerEntityModesDelta
    deriving (Eq, Show, Generic)

instance ToJSON DealerEntityDelta

instance FromJSON DealerEntityDelta

instance Incremental DealerEntityDelta where
    type Target DealerEntityDelta = DealerEntity

    applyDelta delta entity = case delta of
        DealerEntityAttrsDelta d ->
            entity{_dealerEntityAttrs = applyDelta d (_dealerEntityAttrs entity)}
        DealerEntityModesDelta d ->
            entity{_dealerEntityModes = applyDelta d (_dealerEntityModes entity)}
        DealerEntityRelsDelta d ->
            entity{_dealerEntityRels = applyDelta d (_dealerEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        DealerEntityAttrsDelta sd -> describeDelta sd (_dealerEntityAttrs entity)
        DealerEntityModesDelta fd -> describeDelta fd (_dealerEntityModes entity)
        DealerEntityRelsDelta rd -> describeDelta rd (_dealerEntityRels entity)

instance Reversible DealerEntityDelta where
    invert = \case
        DealerEntityModesDelta d -> DealerEntityModesDelta <$> invert d
        DealerEntityAttrsDelta d -> DealerEntityAttrsDelta <$> invert d
        DealerEntityRelsDelta d -> DealerEntityRelsDelta <$> invert d
