{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Delta where

import GHC.Generics (Generic)
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
import Pitboss.Trace.Entity.Types
import Data.Aeson (ToJSON, FromJSON)

data family Delta (k :: EntityKind)

-- And so on for each kind used in your `Trace` type
data instance Delta 'Offering
    = OfferingEntityAttrsDelta OfferingEntityAttrsDelta
    | OfferingEntityModesDelta OfferingEntityModesDelta
    | OfferingEntityRelsDelta OfferingEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Offering)
instance FromJSON (Delta 'Offering)

data instance Delta 'Table
    = TableEntityAttrsDelta TableEntityAttrsDelta
    | TableEntityModesDelta TableEntityModesDelta
    | TableEntityRelsDelta TableEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Table)
instance FromJSON (Delta 'Table)

data instance Delta 'TableShoe
    = Noop
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'TableShoe)
instance FromJSON (Delta 'TableShoe)

data instance Delta 'TableShoeCursor
    = TableShoeCursorEntityAttrsDelta TableShoeCursorEntityAttrsDelta
    | TableShoeCursorEntityModesDelta TableShoeCursorEntityModesDelta
    | TableShoeCursorEntityRelsDelta TableShoeCursorEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'TableShoeCursor)
instance FromJSON (Delta 'TableShoeCursor)

data instance Delta 'Dealer
    = DealerEntityAttrsDelta DealerEntityAttrsDelta
    | DealerEntityModesDelta DealerEntityModesDelta
    | DealerEntityRelsDelta DealerEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Dealer)
instance FromJSON (Delta 'Dealer)

data instance Delta 'DealerRound
    = DealerRoundEntityAttrsDelta DealerRoundEntityAttrsDelta
    | DealerRoundEntityModesDelta DealerRoundEntityModesDelta
    | DealerRoundEntityRelsDelta DealerRoundEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'DealerRound)
instance FromJSON (Delta 'DealerRound)

data instance Delta 'DealerHand
    = DealerHandEntityAttrsDelta DealerHandEntityAttrsDelta
    | DealerHandEntityModesDelta DealerHandEntityModesDelta
    | DealerHandEntityRelsDelta DealerHandEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'DealerHand)
instance FromJSON (Delta 'DealerHand)

data instance Delta 'Player
    = PlayerEntityAttrsDelta PlayerEntityAttrsDelta
    | PlayerEntityModesDelta PlayerEntityModesDelta
    | PlayerEntityRelsDelta PlayerEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Player)
instance FromJSON (Delta 'Player)

data instance Delta 'PlayerSpot
    = PlayerSpotEntityAttrsDelta PlayerSpotEntityAttrsDelta
    | PlayerSpotEntityModesDelta PlayerSpotEntityModesDelta
    | PlayerSpotEntityRelsDelta PlayerSpotEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'PlayerSpot)
instance FromJSON (Delta 'PlayerSpot)

data instance Delta 'PlayerHand
    = PlayerHandEntityAttrsDelta PlayerHandEntityAttrsDelta
    | PlayerHandEntityModesDelta PlayerHandEntityModesDelta
    | PlayerHandEntityRelsDelta PlayerHandEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'PlayerHand)
instance FromJSON (Delta 'PlayerHand)

instance Reversible (Delta 'Offering) where
    invert = \case
        OfferingEntityAttrsDelta d -> OfferingEntityAttrsDelta <$> invert d
        OfferingEntityModesDelta d -> OfferingEntityModesDelta <$> invert d
        OfferingEntityRelsDelta d -> OfferingEntityRelsDelta <$> invert d

instance Reversible (Delta 'Table) where
    invert = \case
        TableEntityAttrsDelta d -> TableEntityAttrsDelta <$> invert d
        TableEntityModesDelta d -> TableEntityModesDelta <$> invert d
        TableEntityRelsDelta d -> TableEntityRelsDelta <$> invert d

instance Reversible (Delta 'TableShoe) where
    invert Noop = Right Noop

instance Reversible (Delta 'TableShoeCursor) where
    invert = \case
        TableShoeCursorEntityAttrsDelta d -> TableShoeCursorEntityAttrsDelta <$> invert d
        TableShoeCursorEntityModesDelta d -> TableShoeCursorEntityModesDelta <$> invert d
        TableShoeCursorEntityRelsDelta d -> TableShoeCursorEntityRelsDelta <$> invert d

instance Reversible (Delta 'Dealer) where
    invert = \case
        DealerEntityAttrsDelta d -> DealerEntityAttrsDelta <$> invert d
        DealerEntityModesDelta d -> DealerEntityModesDelta <$> invert d
        DealerEntityRelsDelta d -> DealerEntityRelsDelta <$> invert d

instance Reversible (Delta 'DealerRound) where
    invert = \case
        DealerRoundEntityAttrsDelta d -> DealerRoundEntityAttrsDelta <$> invert d
        DealerRoundEntityModesDelta d -> DealerRoundEntityModesDelta <$> invert d
        DealerRoundEntityRelsDelta d -> DealerRoundEntityRelsDelta <$> invert d

instance Reversible (Delta 'DealerHand) where
    invert = \case
        DealerHandEntityAttrsDelta d -> DealerHandEntityAttrsDelta <$> invert d
        DealerHandEntityModesDelta d -> DealerHandEntityModesDelta <$> invert d
        DealerHandEntityRelsDelta d -> DealerHandEntityRelsDelta <$> invert d

instance Reversible (Delta 'Player) where
    invert = \case
        PlayerEntityAttrsDelta d -> PlayerEntityAttrsDelta <$> invert d
        PlayerEntityModesDelta d -> PlayerEntityModesDelta <$> invert d
        PlayerEntityRelsDelta d -> PlayerEntityRelsDelta <$> invert d

instance Reversible (Delta 'PlayerHand) where
    invert = \case
        PlayerHandEntityAttrsDelta d -> PlayerHandEntityAttrsDelta <$> invert d
        PlayerHandEntityModesDelta d -> PlayerHandEntityModesDelta <$> invert d
        PlayerHandEntityRelsDelta d -> PlayerHandEntityRelsDelta <$> invert d

instance Reversible (Delta 'PlayerSpot) where
    invert = \case
        PlayerSpotEntityAttrsDelta d -> PlayerSpotEntityAttrsDelta <$> invert d
        PlayerSpotEntityModesDelta d -> PlayerSpotEntityModesDelta <$> invert d
        PlayerSpotEntityRelsDelta d -> PlayerSpotEntityRelsDelta <$> invert d
