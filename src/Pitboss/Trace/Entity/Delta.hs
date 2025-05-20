{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
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

data family Delta (k :: EntityKind)

data instance Delta 'DealerEntity
    = DealerEntityAttrsDelta DealerEntityAttrsDelta
    | DealerEntityModesDelta DealerEntityModesDelta
    | DealerEntityRelsDelta DealerEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'DealerEntity)
instance FromJSON (Delta 'DealerEntity)

data instance Delta 'DealerHandEntity
    = DealerHandEntityAttrsDelta DealerHandEntityAttrsDelta
    | DealerHandEntityModesDelta DealerHandEntityModesDelta
    | DealerHandEntityRelsDelta DealerHandEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'DealerHandEntity)
instance FromJSON (Delta 'DealerHandEntity)

data instance Delta 'DealerRoundEntity
    = DealerRoundEntityAttrsDelta DealerRoundEntityAttrsDelta
    | DealerRoundEntityModesDelta DealerRoundEntityModesDelta
    | DealerRoundEntityRelsDelta DealerRoundEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'DealerRoundEntity)
instance FromJSON (Delta 'DealerRoundEntity)

data instance Delta 'OfferingEntity
    = OfferingEntityAttrsDelta OfferingEntityAttrsDelta
    | OfferingEntityModesDelta OfferingEntityModesDelta
    | OfferingEntityRelsDelta OfferingEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'OfferingEntity)
instance FromJSON (Delta 'OfferingEntity)

data instance Delta 'PlayerEntity
    = PlayerEntityAttrsDelta PlayerEntityAttrsDelta
    | PlayerEntityModesDelta PlayerEntityModesDelta
    | PlayerEntityRelsDelta PlayerEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'PlayerEntity)
instance FromJSON (Delta 'PlayerEntity)

data instance Delta 'PlayerHandEntity
    = PlayerHandEntityAttrsDelta PlayerHandEntityAttrsDelta
    | PlayerHandEntityModesDelta PlayerHandEntityModesDelta
    | PlayerHandEntityRelsDelta PlayerHandEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'PlayerHandEntity)
instance FromJSON (Delta 'PlayerHandEntity)

data instance Delta 'PlayerSpotEntity
    = PlayerSpotEntityAttrsDelta PlayerSpotEntityAttrsDelta
    | PlayerSpotEntityModesDelta PlayerSpotEntityModesDelta
    | PlayerSpotEntityRelsDelta PlayerSpotEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'PlayerSpotEntity)
instance FromJSON (Delta 'PlayerSpotEntity)

data instance Delta 'TableEntity
    = TableEntityAttrsDelta TableEntityAttrsDelta
    | TableEntityModesDelta TableEntityModesDelta
    | TableEntityRelsDelta TableEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'TableEntity)
instance FromJSON (Delta 'TableEntity)

data instance Delta 'TableShoeEntity
    = Noop
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'TableShoeEntity)
instance FromJSON (Delta 'TableShoeEntity)

data instance Delta 'TableShoeCursorEntity
    = TableShoeCursorEntityAttrsDelta TableShoeCursorEntityAttrsDelta
    | TableShoeCursorEntityModesDelta TableShoeCursorEntityModesDelta
    | TableShoeCursorEntityRelsDelta TableShoeCursorEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'TableShoeCursorEntity)
instance FromJSON (Delta 'TableShoeCursorEntity)
