{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.Trace.Entity.Table.Delta
  ( module Pitboss.Trace.Entity.Table.Delta.Attrs,
    module Pitboss.Trace.Entity.Table.Delta.Modes,
    module Pitboss.Trace.Entity.Table.Delta.Rels,
    TableDelta (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.Entity.Table.Delta.Attrs
import Pitboss.Trace.Entity.Table.Delta.Modes
import Pitboss.Trace.Entity.Table.Delta.Rels

data TableDelta
  = TableStateDelta TableStateDelta
  | TableRelationsDelta TableRelationsDelta
  deriving (Eq, Show, Generic)

instance ToJSON TableDelta

instance FromJSON TableDelta

instance Incremental TableDelta where
  type Entity TableDelta = Table

  applyDelta delta entity = case delta of
    TableStateDelta d -> entity {_tableState = applyDelta d (_tableState entity)}
    TableRelationsDelta d -> entity {_tableRels = applyDelta d (_tableRels entity)}

  previewDelta delta entity = case delta of
    TableStateDelta _ -> Just $ applyDelta delta entity
    TableRelationsDelta _ -> Just $ applyDelta delta entity

  describeDelta = describeDelta

instance Reversible TableDelta where
  invert = \case
    TableStateDelta d -> TableStateDelta <$> invert d
    TableRelationsDelta d -> TableRelationsDelta <$> invert d
