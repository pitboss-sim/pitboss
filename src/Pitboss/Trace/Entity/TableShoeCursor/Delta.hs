{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.Trace.Entity.TableShoeCursor.Delta
  ( module Pitboss.Trace.Entity.TableShoeCursor.Delta.Attrs,
    module Pitboss.Trace.Entity.TableShoeCursor.Delta.Modes,
    module Pitboss.Trace.Entity.TableShoeCursor.Delta.Rels,
    TableShoeCursorEntityDelta (..),
  )
where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.TableShoeCursor
import Pitboss.Trace.Entity.TableShoeCursor.Delta.Attrs
import Pitboss.Trace.Entity.TableShoeCursor.Delta.Modes
import Pitboss.Trace.Entity.TableShoeCursor.Delta.Rels

data TableShoeCursorEntityDelta
  = TableShoeCursorEntityAttrsDelta TableShoeCursorEntityAttrsDelta
  | TableShoeCursorEntityModesDelta TableShoeCursorEntityModesDelta
  | TableShoeCursorEntityRelsDelta TableShoeCursorEntityRelsDelta
  deriving (Eq, Show, Generic)

instance Incremental TableShoeCursorEntityDelta where
  type Target TableShoeCursorEntityDelta = TableShoeCursorEntity

  applyDelta delta entity = case delta of
    TableShoeCursorEntityAttrsDelta d ->
      entity {_tableShoeCursorEntityAttrs = applyDelta d (_tableShoeCursorEntityAttrs entity)}
    TableShoeCursorEntityModesDelta d ->
      entity {_tableShoeCursorEntityModes = applyDelta d (_tableShoeCursorEntityModes entity)}
    TableShoeCursorEntityRelsDelta d ->
      entity {_tableShoeCursorEntityRels = applyDelta d (_tableShoeCursorEntityRels entity)}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    TableShoeCursorEntityAttrsDelta _ -> describeDelta delta entity
    TableShoeCursorEntityModesDelta _ -> describeDelta delta entity
    TableShoeCursorEntityRelsDelta (UpdateTableShoe old new) ->
      "Updated cursor shoe ref: " ++ show old ++ " → " ++ show new

instance Reversible TableShoeCursorEntityDelta where
  invert = \case
    TableShoeCursorEntityAttrsDelta d -> TableShoeCursorEntityAttrsDelta <$> invert d
    TableShoeCursorEntityModesDelta d -> TableShoeCursorEntityModesDelta <$> invert d
    TableShoeCursorEntityRelsDelta d -> TableShoeCursorEntityRelsDelta <$> invert d
