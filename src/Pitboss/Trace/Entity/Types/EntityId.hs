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

type EOfferingId = Id 'Offering
type ETableId = Id 'Table
type ETableShoeId = Id 'TableShoe
type EDealerId = Id 'Dealer
type EDealerRoundId = Id 'DealerRound
type EDealerHandId = Id 'DealerHand
type EPlayerId = Id 'Player
type EPlayerSpotId = Id 'PlayerSpot
type EPlayerHandId = Id 'PlayerHand

mkOfferingId :: Uid -> Id 'Offering
mkOfferingId = OfferingId'

mkTableId :: Uid -> Id 'Table
mkTableId = TableId'

mkTableShoeId :: Uid -> Id 'TableShoe
mkTableShoeId = TableShoeId'

mkDealerId :: Uid -> Id 'Dealer
mkDealerId = DealerId'

mkDealerRoundId :: Uid -> Id 'DealerRound
mkDealerRoundId = DealerRoundId'

mkDealerHandId :: Uid -> Id 'DealerHand
mkDealerHandId = DealerHandId'

mkPlayerId :: Uid -> Id 'Player
mkPlayerId = PlayerId'

mkPlayerSpotId :: Uid -> Id 'PlayerSpot
mkPlayerSpotId = PlayerSpotId'

mkPlayerHandId :: Uid -> Id 'PlayerHand
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
