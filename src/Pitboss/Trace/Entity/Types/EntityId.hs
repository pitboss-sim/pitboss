{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Entity.Types.EntityId (
    EntityRef (..),
    ClockedRef (..),
    TimelessRef (..),
    SomeId (..),
    mkOfferingId,
    mkTableId,
    mkTableShoeId,
    mkDealerId,
    mkDealerRoundId,
    mkDealerHandId,
    mkPlayerId,
    mkPlayerSpotId,
    mkPlayerHandId,
    module Pitboss.Trace.Entity.Types.EntityId.Uid,
    OfferingEntityId,
    TableEntityId,
    TableShoeEntityId,
    DealerEntityId,
    DealerRoundEntityId,
    DealerHandEntityId,
    PlayerEntityId,
    PlayerSpotEntityId,
    PlayerHandEntityId,
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.EntityId.Uid

type OfferingEntityId = Id 'OfferingEntity
type TableEntityId = Id 'TableEntity
type TableShoeEntityId = Id 'TableShoeEntity
type DealerEntityId = Id 'DealerEntity
type DealerRoundEntityId = Id 'DealerRoundEntity
type DealerHandEntityId = Id 'DealerHandEntity
type PlayerEntityId = Id 'PlayerEntity
type PlayerSpotEntityId = Id 'PlayerSpotEntity
type PlayerHandEntityId = Id 'PlayerHandEntity

mkOfferingId :: Uid -> Id 'OfferingEntity
mkOfferingId = OfferingId'

mkTableId :: Uid -> Id 'TableEntity
mkTableId = TableId'

mkTableShoeId :: Uid -> Id 'TableShoeEntity
mkTableShoeId = TableShoeId'

mkDealerId :: Uid -> Id 'DealerEntity
mkDealerId = DealerId'

mkDealerRoundId :: Uid -> Id 'DealerRoundEntity
mkDealerRoundId = DealerRoundId'

mkDealerHandId :: Uid -> Id 'DealerHandEntity
mkDealerHandId = DealerHandId'

mkPlayerId :: Uid -> Id 'PlayerEntity
mkPlayerId = PlayerId'

mkPlayerSpotId :: Uid -> Id 'PlayerSpotEntity
mkPlayerSpotId = PlayerSpotId'

mkPlayerHandId :: Uid -> Id 'PlayerHandEntity
mkPlayerHandId = PlayerHandId'

newtype TimelessRef id = TimelessRef id
    deriving (Eq, Ord, Show, Generic)

data ClockedRef id = ClockedRef Tick id
    deriving (Eq, Ord, Show, Generic)

data EntityRef id
    = Timeless (TimelessRef id)
    | Clocked (ClockedRef id)
    deriving (Eq, Ord, Show, Generic)

instance (ToJSON id, Generic id) => ToJSON (TimelessRef id)
instance (FromJSON id, Generic id) => FromJSON (TimelessRef id)

instance (ToJSON id, Generic id) => ToJSON (ClockedRef id)
instance (FromJSON id, Generic id) => FromJSON (ClockedRef id)

instance (ToJSON id, Generic id) => ToJSON (EntityRef id)
instance (FromJSON id, Generic id) => FromJSON (EntityRef id)
