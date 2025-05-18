{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerSpot.Delta (
    module Pitboss.Trace.Entity.PlayerSpot.Delta.Attrs,
    module Pitboss.Trace.Entity.PlayerSpot.Delta.Modes,
    module Pitboss.Trace.Entity.PlayerSpot.Delta.Rels,
    PlayerSpotEntityDelta (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerSpot.Delta.Attrs
import Pitboss.Trace.Entity.PlayerSpot.Delta.Modes
import Pitboss.Trace.Entity.PlayerSpot.Delta.Rels

data PlayerSpotEntityDelta
    = PlayerSpotEntityAttrsDelta PlayerSpotEntityAttrsDelta
    | PlayerSpotEntityModesDelta PlayerSpotEntityModesDelta
    | PlayerSpotEntityRelsDelta PlayerSpotEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotEntityDelta

instance FromJSON PlayerSpotEntityDelta

instance Incremental PlayerSpotEntityDelta where
    type Target PlayerSpotEntityDelta = PlayerSpotEntity

    applyDelta delta entity = case delta of
        PlayerSpotEntityAttrsDelta d ->
            entity{_playerSpotEntityAttrs = applyDelta d (_playerSpotEntityAttrs entity)}
        PlayerSpotEntityModesDelta d ->
            entity{_playerSpotEntityModes = applyDelta d (_playerSpotEntityModes entity)}
        PlayerSpotEntityRelsDelta d ->
            entity{_playerSpotEntityRels = applyDelta d (_playerSpotEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        PlayerSpotEntityAttrsDelta d -> describeDelta d (_playerSpotEntityAttrs entity)
        PlayerSpotEntityModesDelta d -> describeDelta d (_playerSpotEntityModes entity)
        PlayerSpotEntityRelsDelta d -> describeDelta d (_playerSpotEntityRels entity)

instance Reversible PlayerSpotEntityDelta where
    invert = \case
        PlayerSpotEntityAttrsDelta d -> PlayerSpotEntityAttrsDelta <$> invert d
        PlayerSpotEntityModesDelta d -> PlayerSpotEntityModesDelta <$> invert d
        PlayerSpotEntityRelsDelta d -> PlayerSpotEntityRelsDelta <$> invert d
