{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerHand.Delta (
    module Pitboss.Trace.Entity.PlayerHand.Delta.Attrs,
    module Pitboss.Trace.Entity.PlayerHand.Delta.Modes,
    module Pitboss.Trace.Entity.PlayerHand.Delta.Rels,
    PlayerHandEntityDelta (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerHand.Delta.Attrs
import Pitboss.Trace.Entity.PlayerHand.Delta.Modes
import Pitboss.Trace.Entity.PlayerHand.Delta.Rels

data PlayerHandEntityDelta
    = PlayerHandEntityAttrsDelta PlayerHandEntityAttrsDelta
    | PlayerHandEntityModesDelta PlayerHandEntityModesDelta
    | PlayerHandEntityRelsDelta PlayerHandEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntityDelta

instance FromJSON PlayerHandEntityDelta

instance Incremental PlayerHandEntityDelta where
    type Target PlayerHandEntityDelta = PlayerHandEntity

    applyDelta delta entity = case delta of
        PlayerHandEntityAttrsDelta d ->
            entity{_playerHandEntityAttrs = applyDelta d (_playerHandEntityAttrs entity)}
        PlayerHandEntityModesDelta d ->
            entity{_playerHandEntityModes = applyDelta d (_playerHandEntityModes entity)}
        PlayerHandEntityRelsDelta d ->
            entity{_playerHandEntityRels = applyDelta d (_playerHandEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        PlayerHandEntityAttrsDelta d -> describeDelta d (_playerHandEntityAttrs entity)
        PlayerHandEntityModesDelta d -> describeDelta d (_playerHandEntityModes entity)
        PlayerHandEntityRelsDelta d -> describeDelta d (_playerHandEntityRels entity)

instance Reversible PlayerHandEntityDelta where
    invert = \case
        PlayerHandEntityAttrsDelta d -> PlayerHandEntityAttrsDelta <$> invert d
        PlayerHandEntityModesDelta d -> PlayerHandEntityModesDelta <$> invert d
        PlayerHandEntityRelsDelta d -> PlayerHandEntityRelsDelta <$> invert d
