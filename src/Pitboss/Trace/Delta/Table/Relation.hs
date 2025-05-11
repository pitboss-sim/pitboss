{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.Table.Relation where

import Data.Aeson
import Data.Maybe
import GHC.Generics
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.Types.Identifier

data TableRelationsDelta
  = AssignDealer (Maybe DealerId) DealerId
  | UnassignDealer DealerId
  deriving (Eq, Show, Generic)

instance ToJSON TableRelationsDelta

instance FromJSON TableRelationsDelta

instance Incremental TableRelationsDelta where
  type Entity TableRelationsDelta = TableRelations

  applyDelta delta rels = case delta of
    AssignDealer _ new -> rels {_managedByDealer = Just new}
    UnassignDealer _ -> rels {_managedByDealer = Nothing}

  previewDelta delta entity = case delta of
    AssignDealer prev _ ->
      if _managedByDealer entity == prev
        then Just $ applyDelta delta entity
        else Nothing
    UnassignDealer old ->
      if _managedByDealer entity == Just old
        then Just $ applyDelta delta entity
        else Nothing

  describeDelta delta _ = case delta of
    AssignDealer old new ->
      "Assigned dealer: " ++ maybe "None" show old ++ " → " ++ show new
    UnassignDealer old ->
      "Unassigned dealer: " ++ show old

instance Reversible TableRelationsDelta where
  invert = \case
    AssignDealer prev new -> Right (AssignDealer (Just new) (fromMaybe undefined prev))
    UnassignDealer old -> Right (AssignDealer (Just old) old)
