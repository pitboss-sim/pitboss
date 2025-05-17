{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Offering.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Offering
import Pitboss.Trace.Types.Identifier

data OfferingRelationsDelta
  = AddTable TableId
  | RemoveTable TableId
  deriving (Eq, Show, Generic)

instance ToJSON OfferingRelationsDelta

instance FromJSON OfferingRelationsDelta

instance Incremental OfferingRelationsDelta where
  type Entity OfferingRelationsDelta = OfferingRelations

  applyDelta delta rels = case delta of
    AddTable tid -> rels {_offeringRelsAssociatedTables = tid : _offeringRelsAssociatedTables rels}
    RemoveTable tid -> rels {_offeringRelsAssociatedTables = filter (/= tid) (_offeringRelsAssociatedTables rels)}

  previewDelta delta rels = case delta of
    AddTable tid ->
      if tid `notElem` _offeringRelsAssociatedTables rels
        then Just $ applyDelta delta rels
        else Nothing
    RemoveTable tid ->
      if tid `elem` _offeringRelsAssociatedTables rels
        then Just $ applyDelta delta rels
        else Nothing

  describeDelta delta _ = case delta of
    AddTable tid ->
      "Added table to offering: " ++ show tid
    RemoveTable tid ->
      "Removed table from offering: " ++ show tid

instance Reversible OfferingRelationsDelta where
  invert = \case
    AddTable tid -> Right (RemoveTable tid)
    RemoveTable tid -> Right (AddTable tid)
