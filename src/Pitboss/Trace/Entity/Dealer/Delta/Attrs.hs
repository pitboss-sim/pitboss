{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Dealer.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Dealer
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data DealerEntityAttrsDelta
    = RenameDealer String String
    | ReplaceAssignedTable (Maybe (EntityRef TableEntityId)) (Maybe (EntityRef TableEntityId))
    deriving (Eq, Show, Generic)

instance ToJSON DealerEntityAttrsDelta

instance FromJSON DealerEntityAttrsDelta

instance Incremental DealerEntityAttrsDelta where
    type Target DealerEntityAttrsDelta = DealerEntityAttrs

    applyDelta :: DealerEntityAttrsDelta -> DealerEntityAttrs -> DealerEntityAttrs
    applyDelta delta state = case delta of
        RenameDealer _ new -> state{_dealerEntityAttrsName = new}
        ReplaceAssignedTable _ new -> state{_dealerEntityAttrsAssignedTable = new}

    previewDelta :: DealerEntityAttrsDelta -> DealerEntityAttrs -> Maybe DealerEntityAttrs
    previewDelta delta state = case delta of
        RenameDealer _ _ -> Just $ applyDelta delta state
        ReplaceAssignedTable old _ ->
            if _dealerEntityAttrsAssignedTable state == old
                then Just $ applyDelta delta state
                else Nothing

    describeDelta :: DealerEntityAttrsDelta -> DealerEntityAttrs -> String
    describeDelta delta _ = case delta of
        RenameDealer old new ->
            "Renamed dealer: " ++ old ++ " → " ++ new
        ReplaceAssignedTable old new ->
            "Reassigned dealer table: " ++ show old ++ " → " ++ show new

instance Reversible DealerEntityAttrsDelta where
    invert = \case
        RenameDealer old new -> Right (RenameDealer new old)
        ReplaceAssignedTable old new -> Right (ReplaceAssignedTable new old)
