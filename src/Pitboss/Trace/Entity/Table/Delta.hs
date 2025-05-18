{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Table.Delta (
    module Pitboss.Trace.Entity.Table.Delta.Attrs,
    module Pitboss.Trace.Entity.Table.Delta.Modes,
    module Pitboss.Trace.Entity.Table.Delta.Rels,
    TableEntityDelta (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Table.Delta.Attrs
import Pitboss.Trace.Entity.Table.Delta.Modes
import Pitboss.Trace.Entity.Table.Delta.Rels

data TableEntityDelta
    = TableEntityAttrsDelta TableEntityAttrsDelta
    | TableEntityModesDelta TableEntityModesDelta
    | TableEntityRelsDelta TableEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON TableEntityDelta

instance FromJSON TableEntityDelta

instance Incremental TableEntityDelta where
    type Target TableEntityDelta = TableEntity

    applyDelta delta entity = case delta of
        TableEntityAttrsDelta d -> entity{_tableEntityAttrs = applyDelta d (_tableEntityAttrs entity)}
        TableEntityModesDelta d -> entity{_tableEntityModes = applyDelta d (_tableEntityModes entity)}
        TableEntityRelsDelta d -> entity{_tableEntityRels = applyDelta d (_tableEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        TableEntityAttrsDelta d -> describeDelta d (_tableEntityAttrs entity)
        TableEntityModesDelta d -> describeDelta d (_tableEntityModes entity)
        TableEntityRelsDelta d -> describeDelta d (_tableEntityRels entity)

instance Reversible TableEntityDelta where
    invert = \case
        TableEntityAttrsDelta d -> TableEntityAttrsDelta <$> invert d
        TableEntityModesDelta d -> TableEntityModesDelta <$> invert d
        TableEntityRelsDelta d -> TableEntityRelsDelta <$> invert d
