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

data TableEntityRelsDelta
  = AssignDealer (Maybe (EntityRef DealerEntityId)) (EntityRef DealerEntityId)
  | UnassignDealer (EntityRef DealerEntityId)
  deriving (Eq, Show, Generic)

instance ToJSON TableEntityRelsDelta

instance FromJSON TableEntityRelsDelta

instance Incremental TableEntityRelsDelta where
  type Entity TableEntityRelsDelta = TableEntityRels

  applyDelta delta rels = case delta of
    AssignDealer _ new -> rels {_tableEntityRelsManagedByDealer = pure new}
    UnassignDealer _ -> rels {_tableEntityRelsManagedByDealer = Nothing}

  previewDelta delta entity = case delta of
    AssignDealer old _ ->
      if _tableEntityRelsManagedByDealer entity == old
        then Just $ applyDelta delta entity
        else Nothing
    UnassignDealer old -> case _tableEntityRelsManagedByDealer entity of
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

instance Reversible TableEntityRelsDelta where
  invert = \case
    AssignDealer prev new -> Right (AssignDealer (Just new) (fromMaybe undefined prev))
    UnassignDealer old -> Right (AssignDealer (Just old) old)
