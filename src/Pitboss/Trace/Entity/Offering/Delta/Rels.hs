{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Offering.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Offering
import Pitboss.Trace.Types.Identifier

data OfferingEntityRelsDelta
    = AddTable TableEntityId
    | RemoveTable TableEntityId
    deriving (Eq, Show, Generic)

instance ToJSON OfferingEntityRelsDelta
instance FromJSON OfferingEntityRelsDelta

instance Incremental OfferingEntityRelsDelta where
    type Target OfferingEntityRelsDelta = OfferingEntityRels

    applyDelta :: OfferingEntityRelsDelta -> OfferingEntityRels -> OfferingEntityRels
    applyDelta delta rels = case delta of
        AddTable tid -> rels{_offeringEntityRelsAssociatedTables = tid : _offeringEntityRelsAssociatedTables rels}
        RemoveTable tid -> rels{_offeringEntityRelsAssociatedTables = filter (/= tid) (_offeringEntityRelsAssociatedTables rels)}

    previewDelta :: OfferingEntityRelsDelta -> OfferingEntityRels -> Maybe OfferingEntityRels
    previewDelta delta rels = case delta of
        AddTable tid ->
            if tid `notElem` _offeringEntityRelsAssociatedTables rels
                then Just $ applyDelta delta rels
                else Nothing
        RemoveTable tid ->
            if tid `elem` _offeringEntityRelsAssociatedTables rels
                then Just $ applyDelta delta rels
                else Nothing

    describeDelta :: OfferingEntityRelsDelta -> OfferingEntityRels -> String
    describeDelta delta _ = case delta of
        AddTable tid -> "Added table to offering: " ++ show tid
        RemoveTable tid -> "Removed table from offering: " ++ show tid

instance Reversible OfferingEntityRelsDelta where
    invert = \case
        AddTable tid -> Right (RemoveTable tid)
        RemoveTable tid -> Right (AddTable tid)
