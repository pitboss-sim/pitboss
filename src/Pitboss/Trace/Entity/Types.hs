{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Types where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, Value, object, withObject, (.:), (.=))
import Data.Hashable (Hashable (..))
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types.EntityId.Uid

newtype Tick = Tick Word64
    deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

class HasUid a where
    getUid :: a -> Uid

instance HasUid (Id k) where
    getUid (OfferingId' u) = u
    getUid (TableId' u) = u
    getUid (TableShoeId' u) = u
    getUid (DealerId' u) = u
    getUid (DealerRoundId' u) = u
    getUid (DealerHandId' u) = u
    getUid (PlayerId' u) = u
    getUid (PlayerSpotId' u) = u
    getUid (PlayerHandId' u) = u

instance ToJSON SomeKind
instance FromJSON SomeKind

instance ToJSON EntityKind
instance FromJSON EntityKind
data SomeKind where
    SomeKind :: EntityKind -> SomeKind
    deriving (Generic)

data EntityKind
    = Dealer
    | DealerHand
    | DealerRound
    | Offering
    | Player
    | PlayerHand
    | PlayerSpot
    | Table
    | TableShoe
    deriving (Eq, Show, Generic)

data SomeId where
    SomeId :: Id k -> SomeId

data Id (k :: EntityKind)
    = OfferingId' Uid
    | TableId' Uid
    | TableShoeId' Uid
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
        DealerId' u -> tagged "Dealer" u
        DealerRoundId' u -> tagged "DealerRound" u
        DealerHandId' u -> tagged "DealerHand" u
        PlayerId' u -> tagged "Player" u
        PlayerSpotId' u -> tagged "PlayerSpot" u
        PlayerHandId' u -> tagged "PlayerHand" u
      where
        tagged :: T.Text -> Uid -> Value
        tagged tag uid = object ["tag" .= tag, "uid" .= uid]

instance FromJSON SomeId where
    parseJSON = withObject "SomeId" $ \o -> do
        tag <- o .: "tag"
        uid <- o .: "uid"
        case tag :: T.Text of
            "Offering" -> pure $ SomeId (OfferingId' uid)
            "Table" -> pure $ SomeId (TableId' uid)
            "TableShoe" -> pure $ SomeId (TableShoeId' uid)
            "Dealer" -> pure $ SomeId (DealerId' uid)
            "DealerRound" -> pure $ SomeId (DealerRoundId' uid)
            "DealerHand" -> pure $ SomeId (DealerHandId' uid)
            "Player" -> pure $ SomeId (PlayerId' uid)
            "PlayerSpot" -> pure $ SomeId (PlayerSpotId' uid)
            "PlayerHand" -> pure $ SomeId (PlayerHandId' uid)
            _ -> fail $ "Unknown tag in SomeId: " ++ T.unpack tag

-- Meta

data Meta id = Meta
    { _id :: id
    , _bornAt :: Maybe Tick
    }
    deriving (Eq, Show, Generic)

instance (ToJSON id) => ToJSON (Meta id)

instance (FromJSON id) => FromJSON (Meta id)

instance (HasUid id) => HasUid (Meta id) where
    getUid = getUid . _id
