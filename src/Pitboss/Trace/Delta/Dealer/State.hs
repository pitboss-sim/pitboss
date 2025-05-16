{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.Dealer.State where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.Dealer
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data DealerStateDelta
  = RenameDealer String String
  | ReplaceAssignedTable (Maybe (EntityRef TableId)) (Maybe (EntityRef TableId))
  deriving (Eq, Show, Generic)

instance ToJSON DealerStateDelta

instance FromJSON DealerStateDelta

instance Incremental DealerStateDelta where
  type Entity DealerStateDelta = DealerState

  applyDelta :: DealerStateDelta -> DealerState -> DealerState
  applyDelta delta state = case delta of
    RenameDealer _ new -> state {_dealerName = new}
    ReplaceAssignedTable _ new -> state {_assignedTable = new}

  previewDelta :: DealerStateDelta -> DealerState -> Maybe DealerState
  previewDelta delta state = case delta of
    RenameDealer _ _ -> Just $ applyDelta delta state
    ReplaceAssignedTable old _ ->
      if _assignedTable state == old
        then Just $ applyDelta delta state
        else Nothing

  describeDelta :: DealerStateDelta -> DealerState -> String
  describeDelta delta _ = case delta of
    RenameDealer old new ->
      "Renamed dealer: " ++ old ++ " → " ++ new
    ReplaceAssignedTable old new ->
      "Reassigned dealer table: " ++ show old ++ " → " ++ show new

instance Reversible DealerStateDelta where
  invert = \case
    RenameDealer old new -> Right (RenameDealer new old)
    ReplaceAssignedTable old new -> Right (ReplaceAssignedTable new old)
