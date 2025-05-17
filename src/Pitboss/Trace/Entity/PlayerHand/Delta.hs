{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerHand.Delta
  ( module Pitboss.Trace.Entity.PlayerHand.Delta.Attrs,
    module Pitboss.Trace.Entity.PlayerHand.Delta.Modes,
    module Pitboss.Trace.Entity.PlayerHand.Delta.Rels,
    PlayerHandEntityDelta (..),
  )
where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerHand
import Pitboss.Trace.Entity.PlayerHand.Delta.Attrs
import Pitboss.Trace.Entity.PlayerHand.Delta.Modes
import Pitboss.Trace.Entity.PlayerHand.Delta.Rels

data PlayerHandEntityDelta
  = PlayerHandEntityAttrsDelta PlayerHandEntityAttrsDelta
  | PlayerHandEntityRelsDelta PlayerHandEntityRelsDelta
  | PlayerHandEntityModesDelta PlayerHandEntityModesDelta
  deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntityDelta

instance FromJSON PlayerHandEntityDelta

instance Incremental PlayerHandEntityDelta where
  type Target PlayerHandEntityDelta = PlayerHandEntity

  applyDelta delta entity = case delta of
    PlayerHandEntityAttrsDelta d ->
      entity {_playerHandEntityAttrs = applyDelta d (_playerHandEntityAttrs entity)}
    PlayerHandEntityRelsDelta d ->
      entity {_playerHandEntityRels = applyDelta d (_playerHandEntityRels entity)}
    PlayerHandEntityModesDelta d ->
      entity {_playerHandEntityModes = applyDelta d (_playerHandEntityModes entity)}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    PlayerHandEntityAttrsDelta sd -> describeDelta sd (_playerHandEntityAttrs entity)
    PlayerHandEntityRelsDelta rd -> describeDelta rd (_playerHandEntityRels entity)
    PlayerHandEntityModesDelta _ -> "FSM replaced"

instance Reversible PlayerHandEntityDelta where
  invert = \case
    PlayerHandEntityAttrsDelta d -> PlayerHandEntityAttrsDelta <$> invert d
    PlayerHandEntityRelsDelta d -> PlayerHandEntityRelsDelta <$> invert d
    PlayerHandEntityModesDelta d -> PlayerHandEntityModesDelta <$> invert d
