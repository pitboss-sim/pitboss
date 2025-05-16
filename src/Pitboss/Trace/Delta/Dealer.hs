{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.Dealer where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Delta.Dealer.FSM
import Pitboss.Trace.Delta.Dealer.Relation
import Pitboss.Trace.Delta.Dealer.State
import Pitboss.Trace.Entity.Dealer

data DealerDelta
  = DealerStateDelta DealerStateDelta
  | DealerRelationsDelta DealerRelationsDelta
  | DealerFSMDelta DealerFSMDelta
  deriving (Eq, Show, Generic)

instance ToJSON DealerDelta

instance FromJSON DealerDelta

instance Incremental DealerDelta where
  type Entity DealerDelta = Dealer

  applyDelta delta entity = case delta of
    DealerStateDelta d ->
      entity {_state = applyDelta d (_state entity)}
    DealerRelationsDelta d ->
      entity {_rels = applyDelta d (_rels entity)}
    DealerFSMDelta d -> applyDelta d entity

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    DealerStateDelta sd -> describeDelta sd (_state entity)
    DealerRelationsDelta rd -> describeDelta rd (_rels entity)
    DealerFSMDelta fd -> describeDelta fd entity

instance Reversible DealerDelta where
  invert = \case
    DealerStateDelta d -> DealerStateDelta <$> invert d
    DealerRelationsDelta d -> DealerRelationsDelta <$> invert d
    DealerFSMDelta d -> DealerFSMDelta <$> invert d
