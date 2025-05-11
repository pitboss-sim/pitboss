{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.Trace.Entity.Table.Delta
  ( module Pitboss.Trace.Entity.Table.Delta.Attrs,
    module Pitboss.Trace.Entity.Table.Delta.Modes,
    module Pitboss.Trace.Entity.Table.Delta.Rels,
    TableEntityDelta (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.Entity.Table.Delta.Attrs
import Pitboss.Trace.Entity.Table.Delta.Modes
import Pitboss.Trace.Entity.Table.Delta.Rels

data TableEntityDelta
  = TableEntityAttrsDelta TableEntityAttrsDelta
  | TableEntityRelsDelta TableEntityRelsDelta
  deriving (Eq, Show, Generic)

instance ToJSON TableEntityDelta

instance FromJSON TableEntityDelta

instance Incremental TableEntityDelta where
  type Entity TableEntityDelta = TableEntity

  applyDelta delta entity = case delta of
    TableEntityAttrsDelta d -> entity {_tableEntityAttrs = applyDelta d (_tableEntityAttrs entity)}
    TableEntityRelsDelta d -> entity {_tableEntityRels = applyDelta d (_tableEntityRels entity)}

  previewDelta delta entity = case delta of
    TableEntityAttrsDelta _ -> Just $ applyDelta delta entity
    TableEntityRelsDelta _ -> Just $ applyDelta delta entity

  describeDelta = describeDelta

instance Reversible TableEntityDelta where
  invert = \case
    TableEntityAttrsDelta d -> TableEntityAttrsDelta <$> invert d
    TableEntityRelsDelta d -> TableEntityRelsDelta <$> invert d
