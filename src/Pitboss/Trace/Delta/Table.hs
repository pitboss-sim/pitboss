{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.Table where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Delta.Table.Relation
import Pitboss.Trace.Delta.Table.State
import Pitboss.Trace.Entity.Table

data TableDelta
  = TableStateDelta TableStateDelta
  | TableRelationsDelta TableRelationsDelta
  deriving (Eq, Show, Generic)

instance ToJSON TableDelta

instance FromJSON TableDelta

instance Incremental TableDelta where
  type Entity TableDelta = Table

  applyDelta delta entity = case delta of
    TableStateDelta d -> entity {_state = applyDelta d (_state entity)}
    TableRelationsDelta d -> entity {_rels = applyDelta d (_rels entity)}

  previewDelta delta entity = case delta of
    TableStateDelta _ -> Just $ applyDelta delta entity
    TableRelationsDelta _ -> Just $ applyDelta delta entity

  describeDelta = describeDelta

instance Reversible TableDelta where
  invert = \case
    TableStateDelta d -> TableStateDelta <$> invert d
    TableRelationsDelta d -> TableRelationsDelta <$> invert d
