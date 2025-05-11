{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.DealerRound where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Delta.DealerRound.Relation
import Pitboss.Trace.Delta.DealerRound.State
import Pitboss.Trace.Entity.DealerRound

data DealerRoundDelta
  = DealerRoundStateDelta DealerRoundStateDelta
  | DealerRoundRelationsDelta DealerRoundRelationsDelta
  deriving (Eq, Show, Generic)

instance ToJSON DealerRoundDelta

instance FromJSON DealerRoundDelta

instance Incremental DealerRoundDelta where
  type Entity DealerRoundDelta = DealerRound

  applyDelta delta entity = case delta of
    DealerRoundStateDelta rd -> entity {_state = applyDelta rd (_state entity)}
    DealerRoundRelationsDelta sd -> entity {_rels = applyDelta sd (_rels entity)}

  previewDelta delta e = Just $ applyDelta delta e

  describeDelta delta _ = case delta of
    DealerRoundStateDelta (SetDealerRoundNumber n) -> "Set round number to " ++ show n
    DealerRoundStateDelta (SetActive b) -> "Set round active: " ++ show b
    DealerRoundRelationsDelta _ -> "Updated shoe used"

instance Reversible DealerRoundDelta where
  invert = \case
    DealerRoundStateDelta delta -> DealerRoundStateDelta <$> invert delta
    DealerRoundRelationsDelta delta -> DealerRoundRelationsDelta <$> invert delta
