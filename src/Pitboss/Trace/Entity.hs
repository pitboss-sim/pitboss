module Pitboss.Trace.Entity (
    module Pitboss.Trace.Entity.Types,
    mkDealerEntity,
    DealerEntity (..),
    mkDealerHandEntity,
    DealerHandEntity (..),
    mkDealerRoundEntity,
    DealerRoundEntity (..),
    mkOffering,
    OfferingEntity (..),
    mkPlayerEntity,
    PlayerEntity (..),
    mkPlayerHandEntity,
    PlayerHandEntity (..),
    mkPlayerSpotEntity,
    PlayerSpotEntity (..),
    mkTableEntity,
    TableEntity (..),
    mkTableShoeEntity,
    TableShoeEntity (..),
    mkTableShoeCursor,
    TableShoeCursorEntity (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Dealer.Types
import Pitboss.Trace.Entity.DealerHand.Types
import Pitboss.Trace.Entity.DealerRound.Types
import Pitboss.Trace.Entity.Offering.Types
import Pitboss.Trace.Entity.Player.Types
import Pitboss.Trace.Entity.PlayerHand.Types
import Pitboss.Trace.Entity.PlayerSpot.Types
import Pitboss.Trace.Entity.Table.Types
import Pitboss.Trace.Entity.TableShoe.Types
import Pitboss.Trace.Entity.TableShoeCursor.Types
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkDealerEntity :: Meta (EntityRef DealerEntityId) -> DealerEntityAttrs -> DealerEntityModes -> DealerEntityRels -> DealerEntity
mkDealerEntity = DealerEntity

mkDealerHandEntity :: Meta (EntityRef DealerHandEntityId) -> DealerHandEntityAttrs -> DealerHandEntityModes -> DealerHandEntityRels -> DealerHandEntity
mkDealerHandEntity = DealerHandEntity

mkDealerRoundEntity :: Meta DealerRoundEntityId -> DealerRoundEntityAttrs -> DealerRoundEntityModes -> DealerRoundEntityRels -> DealerRoundEntity
mkDealerRoundEntity = DealerRoundEntity

mkOffering :: Meta OfferingEntityId -> OfferingEntityAttrs -> OfferingEntityModes -> OfferingEntityRels -> OfferingEntity
mkOffering = OfferingEntity

mkPlayerEntity :: Meta (EntityRef PlayerEntityId) -> PlayerEntityAttrs -> PlayerEntityModes -> PlayerEntityRels -> PlayerEntity
mkPlayerEntity = PlayerEntity

mkPlayerHandEntity :: Meta (EntityRef PlayerHandEntityId) -> PlayerHandEntityAttrs -> PlayerHandEntityModes -> PlayerHandEntityRels -> PlayerHandEntity
mkPlayerHandEntity = PlayerHandEntity

mkPlayerSpotEntity :: Meta (EntityRef PlayerSpotEntityId) -> PlayerSpotEntityAttrs -> PlayerSpotEntityModes -> PlayerSpotEntityRels -> PlayerSpotEntity
mkPlayerSpotEntity = PlayerSpotEntity

mkTableEntity :: Meta TableEntityId -> TableEntityAttrs -> TableEntityModes -> TableEntityRels -> TableEntity
mkTableEntity = TableEntity

mkTableShoeEntity :: Meta (EntityRef TableShoeEntityId) -> TableShoeEntityAttrs -> TableShoeEntityModes -> TableShoeEntityRels -> TableShoeEntity
mkTableShoeEntity = TableShoeEntity

mkTableShoeCursor :: Meta (EntityRef TableShoeCursorEntityId) -> TableShoeCursorEntityAttrs -> TableShoeCursorEntityModes -> TableShoeCursorEntityRels -> TableShoeCursorEntity
mkTableShoeCursor = TableShoeCursorEntity
data DealerEntity = DealerEntity
    { _dealerEntityMeta :: Meta (EntityRef DealerEntityId)
    , _dealerEntityAttrs :: DealerEntityAttrs
    , _dealerEntityModes :: DealerEntityModes
    , _dealerEntityRels :: DealerEntityRels
    }
    deriving (Eq, Show, Generic)

data DealerHandEntity = DealerHandEntity
    { _dealerHandEntityMeta :: Meta (EntityRef DealerHandEntityId)
    , _dealerHandEntityAttrs :: DealerHandEntityAttrs
    , _dealerHandEntityModes :: DealerHandEntityModes
    , _dealerHandEntityRels :: DealerHandEntityRels
    }
    deriving (Eq, Show, Generic)

data DealerRoundEntity = DealerRoundEntity
    { _dealerRoundEntityMeta :: Meta DealerRoundEntityId
    , _dealerRoundEntityAttrs :: DealerRoundEntityAttrs
    , _dealerRoundEntityModes :: DealerRoundEntityModes
    , _dealerRoundEntityRels :: DealerRoundEntityRels
    }
    deriving (Eq, Show, Generic)

data OfferingEntity = OfferingEntity
    { _offeringEntityMeta :: Meta OfferingEntityId
    , _offeringEntityAttrs :: OfferingEntityAttrs
    , _offeringEntityModes :: OfferingEntityModes
    , _offeringEntityRels :: OfferingEntityRels
    }
    deriving (Eq, Show, Generic)

data PlayerEntity = PlayerEntity
    { _playerEntityMeta :: Meta (EntityRef PlayerEntityId)
    , _playerEntityAttrs :: PlayerEntityAttrs
    , _playerEntityModes :: PlayerEntityModes
    , _playerEntityRels :: PlayerEntityRels
    }
    deriving (Show, Eq, Generic)

data PlayerHandEntity = PlayerHandEntity
    { _playerHandEntityMeta :: Meta (EntityRef PlayerHandEntityId)
    , _playerHandEntityAttrs :: PlayerHandEntityAttrs
    , _playerHandEntityModes :: PlayerHandEntityModes
    , _playerHandEntityRels :: PlayerHandEntityRels
    }
    deriving (Eq, Show, Generic)

data PlayerSpotEntity = PlayerSpotEntity
    { _playerSpotEntityMeta :: Meta (EntityRef PlayerSpotEntityId)
    , _playerSpotEntityAttrs :: PlayerSpotEntityAttrs
    , _playerSpotEntityModes :: PlayerSpotEntityModes
    , _playerSpotEntityRels :: PlayerSpotEntityRels
    }
    deriving (Eq, Show, Generic)

data TableEntity = TableEntity
    { _tableEntityMeta :: Meta TableEntityId
    , _tableEntityAttrs :: TableEntityAttrs
    , _tableEntityModes :: TableEntityModes
    , _tableEntityRels :: TableEntityRels
    }
    deriving (Eq, Show, Generic)

data TableShoeEntity = TableShoeEntity
    { _shoeEntityMeta :: Meta (EntityRef TableShoeEntityId)
    , _shoeEntityAttrs :: TableShoeEntityAttrs
    , _shoeEntityModes :: TableShoeEntityModes
    , _shoeEntityRels :: TableShoeEntityRels
    }
    deriving (Eq, Show, Generic)

data TableShoeCursorEntity = TableShoeCursorEntity
    { _tableShoeCursorEntityMeta :: Meta (EntityRef TableShoeCursorEntityId)
    , _tableShoeCursorEntityAttrs :: TableShoeCursorEntityAttrs
    , _tableShoeCursorEntityModes :: TableShoeCursorEntityModes
    , _tableShoeCursorEntityRels :: TableShoeCursorEntityRels
    }
    deriving (Eq, Show, Generic)

instance ToJSON DealerEntity
instance FromJSON DealerEntity

instance ToJSON DealerHandEntity
instance FromJSON DealerHandEntity

instance ToJSON DealerRoundEntity
instance FromJSON DealerRoundEntity

instance ToJSON OfferingEntity
instance FromJSON OfferingEntity

instance ToJSON PlayerEntity
instance FromJSON PlayerEntity

instance ToJSON PlayerHandEntity
instance FromJSON PlayerHandEntity

instance ToJSON PlayerSpotEntity
instance FromJSON PlayerSpotEntity

instance ToJSON TableEntity
instance FromJSON TableEntity

instance ToJSON TableShoeEntity
instance FromJSON TableShoeEntity

instance ToJSON TableShoeCursorEntity
instance FromJSON TableShoeCursorEntity
