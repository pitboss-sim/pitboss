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
    EOfferingId,
    ETableId,
    ETableShoeId,
    EDealerId,
    EDealerRoundId,
    EDealerHandId,
    EPlayerId,
    EPlayerSpotId,
    EPlayerHandId,
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.EntityId.Uid

type EOfferingId = Id 'EOffering
type ETableId = Id 'ETable
type ETableShoeId = Id 'ETableShoe
type EDealerId = Id 'EDealer
type EDealerRoundId = Id 'EDealerRound
type EDealerHandId = Id 'EDealerHand
type EPlayerId = Id 'EPlayer
type EPlayerSpotId = Id 'EPlayerSpot
type EPlayerHandId = Id 'EPlayerHand

mkOfferingId :: Uid -> Id 'EOffering
mkOfferingId = OfferingId'

mkTableId :: Uid -> Id 'ETable
mkTableId = TableId'

mkTableShoeId :: Uid -> Id 'ETableShoe
mkTableShoeId = TableShoeId'

mkDealerId :: Uid -> Id 'EDealer
mkDealerId = DealerId'

mkDealerRoundId :: Uid -> Id 'EDealerRound
mkDealerRoundId = DealerRoundId'

mkDealerHandId :: Uid -> Id 'EDealerHand
mkDealerHandId = DealerHandId'

mkPlayerId :: Uid -> Id 'EPlayer
mkPlayerId = PlayerId'

mkPlayerSpotId :: Uid -> Id 'EPlayerSpot
mkPlayerSpotId = PlayerSpotId'

mkPlayerHandId :: Uid -> Id 'EPlayerHand
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
