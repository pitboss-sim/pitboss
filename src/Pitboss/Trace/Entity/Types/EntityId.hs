{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Trace.Entity.Types.EntityId (
    EntityRef (..),
    ClockedRef (..),
    TimelessRef (..),
    SomeId (..),
    mkOfferingId,
    mkTableId,
    mkTableShoeId,
    mkTableShoeCursorId,
    mkDealerId,
    mkDealerRoundId,
    mkDealerHandId,
    mkPlayerId,
    mkPlayerSpotId,
    mkPlayerHandId,
    OfferingEntityId,
    TableEntityId,
    TableShoeEntityId,
    TableShoeCursorEntityId,
    DealerEntityId,
    DealerRoundEntityId,
    DealerHandEntityId,
    PlayerEntityId,
    PlayerSpotEntityId,
    PlayerHandEntityId,
) where

import Data.Aeson (FromJSON (..), KeyValue (..), ToJSON (..), Value, object, withObject, (.:))
import Data.Text
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Types.Kind
import Pitboss.Trace.Entity.Types.Uid

type OfferingEntityId = Id 'Offering
type TableEntityId = Id 'Table
type TableShoeEntityId = Id 'TableShoe
type TableShoeCursorEntityId = Id 'TableShoeCursor
type DealerEntityId = Id 'Dealer
type DealerRoundEntityId = Id 'DealerRound
type DealerHandEntityId = Id 'DealerHand
type PlayerEntityId = Id 'Player
type PlayerSpotEntityId = Id 'PlayerSpot
type PlayerHandEntityId = Id 'PlayerHand

mkOfferingId :: Uid -> Id 'Offering
mkOfferingId = OfferingId'

mkTableId :: Uid -> Id 'Table
mkTableId = TableId'

mkTableShoeId :: Uid -> Id 'TableShoe
mkTableShoeId = TableShoeId'

mkTableShoeCursorId :: Uid -> Id 'TableShoe
mkTableShoeCursorId = TableShoeId'

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

data SomeId where
    SomeId :: Id k -> SomeId

data Id (k :: EntityKind)
    = OfferingId' Uid
    | TableId' Uid
    | TableShoeId' Uid
    | TableShoeCursorId' Uid
    | DealerId' Uid
    | DealerRoundId' Uid
    | DealerHandId' Uid
    | PlayerId' Uid
    | PlayerSpotId' Uid
    | PlayerHandId' Uid

deriving instance Show (Id k)
deriving instance Eq (Id k)
deriving instance Ord (Id k)
deriving instance Generic (Id k)

instance ToJSON (Id 'Offering)
instance FromJSON (Id 'Offering)

instance ToJSON (Id 'Table)
instance FromJSON (Id 'Table)

instance ToJSON (Id 'TableShoe)
instance FromJSON (Id 'TableShoe)

instance ToJSON (Id 'TableShoeCursor)
instance FromJSON (Id 'TableShoeCursor)

instance ToJSON (Id 'Dealer)
instance FromJSON (Id 'Dealer)

instance ToJSON (Id 'DealerRound)
instance FromJSON (Id 'DealerRound)

instance ToJSON (Id 'DealerHand)
instance FromJSON (Id 'DealerHand)

instance ToJSON (Id 'Player)
instance FromJSON (Id 'Player)

instance ToJSON (Id 'PlayerSpot)
instance FromJSON (Id 'PlayerSpot)

instance ToJSON (Id 'PlayerHand)
instance FromJSON (Id 'PlayerHand)

instance ToJSON SomeId where
    toJSON (SomeId id') = case id' of
        OfferingId' u -> tagged "Offering" u
        TableId' u -> tagged "Table" u
        TableShoeId' u -> tagged "TableShoe" u
        TableShoeCursorId' u -> tagged "TableShoeCursor" u
        DealerId' u -> tagged "Dealer" u
        DealerRoundId' u -> tagged "DealerRound" u
        DealerHandId' u -> tagged "DealerHand" u
        PlayerId' u -> tagged "Player" u
        PlayerSpotId' u -> tagged "PlayerSpot" u
        PlayerHandId' u -> tagged "PlayerHand" u
      where
        tagged :: Text -> Uid -> Value
        tagged tag uid = object ["tag" .= tag, "uid" .= uid]

instance FromJSON SomeId where
    parseJSON = withObject "SomeId" $ \o -> do
        tag <- o .: "tag"
        uid <- o .: "uid"
        case tag :: Text of
            "Offering" -> pure $ SomeId (OfferingId' uid)
            "Table" -> pure $ SomeId (TableId' uid)
            "TableShoe" -> pure $ SomeId (TableShoeId' uid)
            "TableShoeCursor" -> pure $ SomeId (TableShoeCursorId' uid)
            "Dealer" -> pure $ SomeId (DealerId' uid)
            "DealerRound" -> pure $ SomeId (DealerRoundId' uid)
            "DealerHand" -> pure $ SomeId (DealerHandId' uid)
            "Player" -> pure $ SomeId (PlayerId' uid)
            "PlayerSpot" -> pure $ SomeId (PlayerSpotId' uid)
            "PlayerHand" -> pure $ SomeId (PlayerHandId' uid)
            _ -> fail $ "Unknown tag in SomeId: " ++ unpack tag

instance HasUid (Id k) where
    getUid (OfferingId' u) = u
    getUid (TableId' u) = u
    getUid (TableShoeId' u) = u
    getUid (TableShoeCursorId' u) = u
    getUid (DealerId' u) = u
    getUid (DealerRoundId' u) = u
    getUid (DealerHandId' u) = u
    getUid (PlayerId' u) = u
    getUid (PlayerSpotId' u) = u
    getUid (PlayerHandId' u) = u

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
