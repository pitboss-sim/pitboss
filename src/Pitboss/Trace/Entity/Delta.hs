{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Delta (
    OfferingEntityDelta (..),
    TableEntityDelta (..),
    TableShoeEntityDelta (..),
    TableShoeCursorEntityDelta (..),
    DealerEntityDelta (..),
    DealerRoundEntityDelta (..),
    DealerHandEntityDelta (..),
    PlayerEntityDelta (..),
    PlayerSpotEntityDelta (..),
    PlayerHandEntityDelta (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Dealer.Delta
import Pitboss.Trace.Entity.DealerHand.Delta
import Pitboss.Trace.Entity.DealerRound.Delta
import Pitboss.Trace.Entity.Offering.Delta
import Pitboss.Trace.Entity.Player.Delta
import Pitboss.Trace.Entity.PlayerHand.Delta
import Pitboss.Trace.Entity.PlayerSpot.Delta
import Pitboss.Trace.Entity.Table.Delta
import Pitboss.Trace.Entity.TableShoeCursor.Delta

data OfferingEntityDelta
    = OfferingEntityAttrsDelta OfferingEntityAttrsDelta
    | OfferingEntityModesDelta OfferingEntityModesDelta
    | OfferingEntityRelsDelta OfferingEntityRelsDelta
    deriving (Eq, Show, Generic)

data TableEntityDelta
    = TableEntityAttrsDelta TableEntityAttrsDelta
    | TableEntityModesDelta TableEntityModesDelta
    | TableEntityRelsDelta TableEntityRelsDelta
    deriving (Eq, Show, Generic)

data TableShoeEntityDelta
    = Noop
    deriving (Eq, Show, Generic)

data TableShoeCursorEntityDelta
    = TableShoeCursorEntityAttrsDelta TableShoeCursorEntityAttrsDelta
    | TableShoeCursorEntityModesDelta TableShoeCursorEntityModesDelta
    | TableShoeCursorEntityRelsDelta TableShoeCursorEntityRelsDelta
    deriving (Eq, Show, Generic)

data DealerEntityDelta
    = DealerEntityAttrsDelta DealerEntityAttrsDelta
    | DealerEntityRelsDelta DealerEntityRelsDelta
    | DealerEntityModesDelta DealerEntityModesDelta
    deriving (Eq, Show, Generic)

data DealerRoundEntityDelta
    = DealerRoundEntityAttrsDelta DealerRoundEntityAttrsDelta
    | DealerRoundEntityModesDelta DealerRoundEntityModesDelta
    | DealerRoundEntityRelsDelta DealerRoundEntityRelsDelta
    deriving (Eq, Show, Generic)

data DealerHandEntityDelta
    = DealerHandEntityAttrsDelta DealerHandEntityAttrsDelta
    | DealerHandEntityModesDelta DealerHandEntityModesDelta
    | DealerHandEntityRelsDelta DealerHandEntityRelsDelta
    deriving (Eq, Show, Generic)

data PlayerEntityDelta
    = PlayerEntityAttrsDelta PlayerEntityAttrsDelta
    | PlayerEntityModesDelta PlayerEntityModesDelta
    | PlayerEntityRelsDelta PlayerEntityRelsDelta
    deriving (Eq, Show, Generic)

data PlayerSpotEntityDelta
    = PlayerSpotEntityAttrsDelta PlayerSpotEntityAttrsDelta
    | PlayerSpotEntityModesDelta PlayerSpotEntityModesDelta
    | PlayerSpotEntityRelsDelta PlayerSpotEntityRelsDelta
    deriving (Eq, Show, Generic)

data PlayerHandEntityDelta
    = PlayerHandEntityAttrsDelta PlayerHandEntityAttrsDelta
    | PlayerHandEntityModesDelta PlayerHandEntityModesDelta
    | PlayerHandEntityRelsDelta PlayerHandEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON OfferingEntityDelta
instance FromJSON OfferingEntityDelta

instance ToJSON TableEntityDelta
instance FromJSON TableEntityDelta

instance ToJSON TableShoeEntityDelta
instance FromJSON TableShoeEntityDelta

instance ToJSON TableShoeCursorEntityDelta
instance FromJSON TableShoeCursorEntityDelta

instance ToJSON DealerEntityDelta
instance FromJSON DealerEntityDelta

instance ToJSON DealerRoundEntityDelta
instance FromJSON DealerRoundEntityDelta

instance ToJSON DealerHandEntityDelta
instance FromJSON DealerHandEntityDelta

instance ToJSON PlayerEntityDelta
instance FromJSON PlayerEntityDelta

instance ToJSON PlayerSpotEntityDelta
instance FromJSON PlayerSpotEntityDelta

instance ToJSON PlayerHandEntityDelta
instance FromJSON PlayerHandEntityDelta

instance Incremental OfferingEntityDelta where
    type Target OfferingEntityDelta = OfferingEntity

    applyDelta delta entity = case delta of
        OfferingEntityAttrsDelta d ->
            entity{_offeringEntityAttrs = applyDelta d (_offeringEntityAttrs entity)}
        OfferingEntityModesDelta d ->
            entity{_offeringEntityModes = applyDelta d (_offeringEntityModes entity)}
        OfferingEntityRelsDelta d ->
            entity{_offeringEntityRels = applyDelta d (_offeringEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        OfferingEntityAttrsDelta d -> describeDelta d (_offeringEntityAttrs entity)
        OfferingEntityModesDelta d -> describeDelta d (_offeringEntityModes entity)
        OfferingEntityRelsDelta d -> describeDelta d (_offeringEntityRels entity)

instance Incremental TableEntityDelta where
    type Target TableEntityDelta = TableEntity

    applyDelta delta entity = case delta of
        TableEntityAttrsDelta d ->
            entity{_tableEntityAttrs = applyDelta d (_tableEntityAttrs entity)}
        TableEntityModesDelta d ->
            entity{_tableEntityModes = applyDelta d (_tableEntityModes entity)}
        TableEntityRelsDelta d ->
            entity{_tableEntityRels = applyDelta d (_tableEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        TableEntityAttrsDelta d -> describeDelta d (_tableEntityAttrs entity)
        TableEntityModesDelta d -> describeDelta d (_tableEntityModes entity)
        TableEntityRelsDelta d -> describeDelta d (_tableEntityRels entity)

instance Incremental TableShoeEntityDelta where
    type Target TableShoeEntityDelta = TableShoeEntity

    applyDelta Noop e = e

    previewDelta Noop = Just

    describeDelta Noop _ = "Noop (TableShoe is static)"

instance Incremental TableShoeCursorEntityDelta where
    type Target TableShoeCursorEntityDelta = TableShoeCursorEntity

    applyDelta delta entity = case delta of
        TableShoeCursorEntityAttrsDelta d ->
            entity{_tableShoeCursorEntityAttrs = applyDelta d (_tableShoeCursorEntityAttrs entity)}
        TableShoeCursorEntityModesDelta d ->
            entity{_tableShoeCursorEntityModes = applyDelta d (_tableShoeCursorEntityModes entity)}
        TableShoeCursorEntityRelsDelta d ->
            entity{_tableShoeCursorEntityRels = applyDelta d (_tableShoeCursorEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        TableShoeCursorEntityAttrsDelta d -> describeDelta d (_tableShoeCursorEntityAttrs entity)
        TableShoeCursorEntityModesDelta d -> describeDelta d (_tableShoeCursorEntityModes entity)
        TableShoeCursorEntityRelsDelta d -> describeDelta d (_tableShoeCursorEntityRels entity)

instance Incremental DealerEntityDelta where
    type Target DealerEntityDelta = DealerEntity

    applyDelta delta entity = case delta of
        DealerEntityAttrsDelta d ->
            entity{_dealerEntityAttrs = applyDelta d (_dealerEntityAttrs entity)}
        DealerEntityModesDelta d ->
            entity{_dealerEntityModes = applyDelta d (_dealerEntityModes entity)}
        DealerEntityRelsDelta d ->
            entity{_dealerEntityRels = applyDelta d (_dealerEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        DealerEntityAttrsDelta d -> describeDelta d (_dealerEntityAttrs entity)
        DealerEntityModesDelta d -> describeDelta d (_dealerEntityModes entity)
        DealerEntityRelsDelta d -> describeDelta d (_dealerEntityRels entity)

instance Incremental DealerRoundEntityDelta where
    type Target DealerRoundEntityDelta = DealerRoundEntity

    applyDelta delta entity = case delta of
        DealerRoundEntityAttrsDelta d ->
            entity{_dealerRoundEntityAttrs = applyDelta d (_dealerRoundEntityAttrs entity)}
        DealerRoundEntityModesDelta d ->
            entity{_dealerRoundEntityModes = applyDelta d (_dealerRoundEntityModes entity)}
        DealerRoundEntityRelsDelta d ->
            entity{_dealerRoundEntityRels = applyDelta d (_dealerRoundEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        DealerRoundEntityAttrsDelta d -> describeDelta d (_dealerRoundEntityAttrs entity)
        DealerRoundEntityModesDelta d -> describeDelta d (_dealerRoundEntityModes entity)
        DealerRoundEntityRelsDelta d -> describeDelta d (_dealerRoundEntityRels entity)

instance Incremental DealerHandEntityDelta where
    type Target DealerHandEntityDelta = DealerHandEntity

    applyDelta delta entity = case delta of
        DealerHandEntityAttrsDelta d ->
            entity{_dealerHandEntityAttrs = applyDelta d (_dealerHandEntityAttrs entity)}
        DealerHandEntityModesDelta d ->
            entity{_dealerHandEntityModes = applyDelta d (_dealerHandEntityModes entity)}
        DealerHandEntityRelsDelta d ->
            entity{_dealerHandEntityRels = applyDelta d (_dealerHandEntityRels entity)}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta delta entity = case delta of
        DealerHandEntityAttrsDelta d -> describeDelta d (_dealerHandEntityAttrs entity)
        DealerHandEntityModesDelta d -> describeDelta d (_dealerHandEntityModes entity)
        DealerHandEntityRelsDelta d -> describeDelta d (_dealerHandEntityRels entity)

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

instance Reversible OfferingEntityDelta where
    invert = \case
        OfferingEntityAttrsDelta d -> OfferingEntityAttrsDelta <$> invert d
        OfferingEntityModesDelta d -> OfferingEntityModesDelta <$> invert d
        OfferingEntityRelsDelta d -> OfferingEntityRelsDelta <$> invert d

instance Reversible TableEntityDelta where
    invert = \case
        TableEntityAttrsDelta d -> TableEntityAttrsDelta <$> invert d
        TableEntityModesDelta d -> TableEntityModesDelta <$> invert d
        TableEntityRelsDelta d -> TableEntityRelsDelta <$> invert d

instance Reversible TableShoeEntityDelta where
    invert Noop = Right Noop

instance Reversible TableShoeCursorEntityDelta where
    invert = \case
        TableShoeCursorEntityAttrsDelta d -> TableShoeCursorEntityAttrsDelta <$> invert d
        TableShoeCursorEntityModesDelta d -> TableShoeCursorEntityModesDelta <$> invert d
        TableShoeCursorEntityRelsDelta d -> TableShoeCursorEntityRelsDelta <$> invert d

instance Reversible DealerEntityDelta where
    invert = \case
        DealerEntityAttrsDelta d -> DealerEntityAttrsDelta <$> invert d
        DealerEntityModesDelta d -> DealerEntityModesDelta <$> invert d
        DealerEntityRelsDelta d -> DealerEntityRelsDelta <$> invert d

instance Reversible DealerRoundEntityDelta where
    invert = \case
        DealerRoundEntityAttrsDelta d -> DealerRoundEntityAttrsDelta <$> invert d
        DealerRoundEntityModesDelta d -> DealerRoundEntityModesDelta <$> invert d
        DealerRoundEntityRelsDelta d -> DealerRoundEntityRelsDelta <$> invert d

instance Reversible DealerHandEntityDelta where
    invert = \case
        DealerHandEntityAttrsDelta d -> DealerHandEntityAttrsDelta <$> invert d
        DealerHandEntityModesDelta d -> DealerHandEntityModesDelta <$> invert d
        DealerHandEntityRelsDelta d -> DealerHandEntityRelsDelta <$> invert d

instance Reversible PlayerEntityDelta where
    invert = \case
        PlayerEntityAttrsDelta d -> PlayerEntityAttrsDelta <$> invert d
        PlayerEntityModesDelta d -> PlayerEntityModesDelta <$> invert d
        PlayerEntityRelsDelta d -> PlayerEntityRelsDelta <$> invert d

instance Reversible PlayerHandEntityDelta where
    invert = \case
        PlayerHandEntityAttrsDelta d -> PlayerHandEntityAttrsDelta <$> invert d
        PlayerHandEntityModesDelta d -> PlayerHandEntityModesDelta <$> invert d
        PlayerHandEntityRelsDelta d -> PlayerHandEntityRelsDelta <$> invert d

instance Reversible PlayerSpotEntityDelta where
    invert = \case
        PlayerSpotEntityAttrsDelta d -> PlayerSpotEntityAttrsDelta <$> invert d
        PlayerSpotEntityModesDelta d -> PlayerSpotEntityModesDelta <$> invert d
        PlayerSpotEntityRelsDelta d -> PlayerSpotEntityRelsDelta <$> invert d
