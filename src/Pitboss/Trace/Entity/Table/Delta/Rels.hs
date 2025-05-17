{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Table.Delta.Rels where

import Data.Aeson
import Data.Maybe
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data TableRelationsDelta
  = AssignDealer (Maybe (EntityRef DealerId)) (EntityRef DealerId)
  | UnassignDealer (EntityRef DealerId)
  deriving (Eq, Show, Generic)

instance ToJSON TableRelationsDelta

instance FromJSON TableRelationsDelta

instance Incremental TableRelationsDelta where
  type Entity TableRelationsDelta = TableRelations

  applyDelta delta rels = case delta of
    AssignDealer _ new -> rels {_tableRelsManagedByDealer = pure new}
    UnassignDealer _ -> rels {_tableRelsManagedByDealer = Nothing}

  previewDelta delta entity = case delta of
    AssignDealer old _ ->
      if _tableRelsManagedByDealer entity == old
        then Just $ applyDelta delta entity
        else Nothing
    UnassignDealer old -> case _tableRelsManagedByDealer entity of
      Just old' ->
        if old == old'
          then Just $ applyDelta delta entity
          else Nothing
      Nothing -> Nothing

  describeDelta delta _ = case delta of
    AssignDealer old new ->
      "Assigned dealer: " ++ maybe "None" show old ++ " → " ++ show new
    UnassignDealer old ->
      "Unassigned dealer: " ++ show old

instance Reversible TableRelationsDelta where
  invert = \case
    AssignDealer prev new -> Right (AssignDealer (Just new) (fromMaybe undefined prev))
    UnassignDealer old -> Right (AssignDealer (Just old) old)
