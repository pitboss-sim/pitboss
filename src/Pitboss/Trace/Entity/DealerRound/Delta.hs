{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.Trace.Entity.DealerRound.Delta
  ( module Pitboss.Trace.Entity.DealerRound.Delta.Attrs,
    module Pitboss.Trace.Entity.DealerRound.Delta.Modes,
    module Pitboss.Trace.Entity.DealerRound.Delta.Rels,
    DealerRoundDelta (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerRound
import Pitboss.Trace.Entity.DealerRound.Delta.Attrs
import Pitboss.Trace.Entity.DealerRound.Delta.Modes
import Pitboss.Trace.Entity.DealerRound.Delta.Rels

data DealerRoundDelta
  = DealerRoundStateDelta DealerRoundStateDelta
  | DealerRoundRelationsDelta DealerRoundRelationsDelta
  deriving (Eq, Show, Generic)

instance ToJSON DealerRoundDelta

instance FromJSON DealerRoundDelta

instance Incremental DealerRoundDelta where
  type Entity DealerRoundDelta = DealerRound

  applyDelta delta entity = case delta of
    DealerRoundStateDelta rd -> entity {_dealerRoundState = applyDelta rd (_dealerRoundState entity)}
    DealerRoundRelationsDelta sd -> entity {_dealerRoundRels = applyDelta sd (_dealerRoundRels entity)}

  previewDelta delta e = Just $ applyDelta delta e

  describeDelta delta _ = case delta of
    DealerRoundStateDelta (SetDealerRoundNumber n) -> "Set round number to " ++ show n
    DealerRoundStateDelta (SetActive b) -> "Set round active: " ++ show b
    DealerRoundRelationsDelta _ -> "Updated shoe used"

instance Reversible DealerRoundDelta where
  invert = \case
    DealerRoundStateDelta delta -> DealerRoundStateDelta <$> invert delta
    DealerRoundRelationsDelta delta -> DealerRoundRelationsDelta <$> invert delta
