{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Dealer.Delta
  ( module Pitboss.Trace.Entity.Dealer.Delta.Attrs,
    module Pitboss.Trace.Entity.Dealer.Delta.Modes,
    module Pitboss.Trace.Entity.Dealer.Delta.Rels,
    DealerDelta (..),
  )
where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Dealer
import Pitboss.Trace.Entity.Dealer.Delta.Attrs
import Pitboss.Trace.Entity.Dealer.Delta.Modes
import Pitboss.Trace.Entity.Dealer.Delta.Rels

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
      entity {_dealerState = applyDelta d (_dealerState entity)}
    DealerRelationsDelta d ->
      entity {_dealerRels = applyDelta d (_dealerRels entity)}
    DealerFSMDelta d -> applyDelta d entity

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    DealerStateDelta sd -> describeDelta sd (_dealerState entity)
    DealerRelationsDelta rd -> describeDelta rd (_dealerRels entity)
    DealerFSMDelta fd -> describeDelta fd entity

instance Reversible DealerDelta where
  invert = \case
    DealerStateDelta d -> DealerStateDelta <$> invert d
    DealerRelationsDelta d -> DealerRelationsDelta <$> invert d
    DealerFSMDelta d -> DealerFSMDelta <$> invert d
