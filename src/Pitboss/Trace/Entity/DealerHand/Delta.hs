{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerHand.Delta
  ( module Pitboss.Trace.Entity.DealerHand.Delta.Attrs,
    module Pitboss.Trace.Entity.DealerHand.Delta.Modes,
    module Pitboss.Trace.Entity.DealerHand.Delta.Rels,
    DealerHandDelta (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerHand
import Pitboss.Trace.Entity.DealerHand.Delta.Attrs
import Pitboss.Trace.Entity.DealerHand.Delta.Modes
import Pitboss.Trace.Entity.DealerHand.Delta.Rels

data DealerHandDelta
  = DealerHandStateDelta DealerHandStateDelta
  | DealerHandRelationsDelta DealerHandRelationsDelta
  | DealerHandFSMDelta DealerHandFSMDelta
  deriving (Eq, Show, Generic)

instance ToJSON DealerHandDelta

instance FromJSON DealerHandDelta

instance Incremental DealerHandDelta where
  type Entity DealerHandDelta = DealerHand

  applyDelta delta entity = case delta of
    DealerHandStateDelta d -> entity {_dealerHandState = applyDelta d (_dealerHandState entity)}
    DealerHandRelationsDelta d -> entity {_dealerHandRels = applyDelta d (_dealerHandRels entity)}
    DealerHandFSMDelta d -> applyDelta d entity

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    DealerHandStateDelta sd -> describeDelta sd (_dealerHandState entity)
    DealerHandRelationsDelta rd -> describeDelta rd (_dealerHandRels entity)
    DealerHandFSMDelta _ -> "FSM replaced"

instance Reversible DealerHandDelta where
  invert = \case
    DealerHandStateDelta d -> DealerHandStateDelta <$> invert d
    DealerHandRelationsDelta d -> DealerHandRelationsDelta <$> invert d
    DealerHandFSMDelta d -> DealerHandFSMDelta <$> invert d
