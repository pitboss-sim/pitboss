{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.TableShoeCursor.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.TableShoeCursor
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data TableShoeCursorEntityRelsDelta
    = UpdateTableShoe (EntityRef TableShoeEntityId) (EntityRef TableShoeEntityId)
    deriving (Eq, Show, Generic)

instance ToJSON TableShoeCursorEntityRelsDelta
instance FromJSON TableShoeCursorEntityRelsDelta

instance Incremental TableShoeCursorEntityRelsDelta where
    type Target TableShoeCursorEntityRelsDelta = TableShoeCursorEntityRels

    applyDelta delta rels = case delta of
        UpdateTableShoe _ new -> rels{_tableShoeCursorEntityRelsPointsToTableShoe = new}

    previewDelta delta rels = case delta of
        UpdateTableShoe old _ | old == _tableShoeCursorEntityRelsPointsToTableShoe rels -> Just $ applyDelta delta rels
        _ -> Nothing

    describeDelta (UpdateTableShoe old new) _ =
        "Updated cursor shoe ref: " ++ show old ++ " -> " ++ show new

instance Reversible TableShoeCursorEntityRelsDelta where
    invert = \case
        UpdateTableShoe old new -> Right (UpdateTableShoe new old)
