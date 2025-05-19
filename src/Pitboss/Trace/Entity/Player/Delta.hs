{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Player.Delta (
    module Pitboss.Trace.Entity.Player.Delta.Attrs,
    module Pitboss.Trace.Entity.Player.Delta.Modes,
    module Pitboss.Trace.Entity.Player.Delta.Rels,
    PlayerEntityDelta (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Player.Delta.Attrs
import Pitboss.Trace.Entity.Player.Delta.Modes
import Pitboss.Trace.Entity.Player.Delta.Rels

data PlayerEntityDelta
    = PlayerEntityAttrsDelta PlayerEntityAttrsDelta
    | PlayerEntityModesDelta PlayerEntityModesDelta
    | PlayerEntityRelsDelta PlayerEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityDelta

instance FromJSON PlayerEntityDelta

instance Incremental PlayerEntityDelta where
    type Target PlayerEntityDelta = PlayerEntity

    applyDelta delta entity = case delta of
        PlayerEntityAttrsDelta d -> entity{_playerEntityAttrs = applyDelta d (_playerEntityAttrs entity)}
        PlayerEntityModesDelta d -> entity{_playerEntityModes = applyDelta d (_playerEntityModes entity)}
        PlayerEntityRelsDelta d -> entity{_playerEntityRels = applyDelta d (_playerEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        PlayerEntityAttrsDelta d -> describeDelta d (_playerEntityAttrs entity)
        PlayerEntityModesDelta d -> describeDelta d (_playerEntityModes entity)
        PlayerEntityRelsDelta d -> describeDelta d (_playerEntityRels entity)

instance Reversible PlayerEntityDelta where
    invert = \case
        PlayerEntityAttrsDelta d -> PlayerEntityAttrsDelta <$> invert d
        PlayerEntityModesDelta d -> PlayerEntityModesDelta <$> invert d
        PlayerEntityRelsDelta d -> PlayerEntityRelsDelta <$> invert d
