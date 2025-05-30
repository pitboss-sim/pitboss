{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.State.Timeline (
    Timeline (..),
    Meta (..),
    mkTimeline,
) where

import Data.Aeson
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import GHC.Generics (Generic)
import Pitboss.State.Types.Core
import Prelude hiding (id)

mkTimeline :: EntityId k -> Tick -> Timeline k a
mkTimeline entityId birthTick =
    Timeline
        { timelineMeta = Meta entityId birthTick Nothing
        , timelineDeltas = mempty
        }

data family Meta (k :: EntityKind)

data instance Meta k = Meta
    { _metaId :: EntityId k
    , _metaBornAt :: Tick
    , _metaDiedAt :: Maybe Tick
    }
    deriving (Generic, Show, Eq)

instance ToJSON (Meta k) where
    toJSON (Meta entityId born died) =
        object
            [ "entityId" .= entityId
            , "bornAt" .= born
            , "diedAt" .= died
            ]

instance FromJSON (Meta k) where
    parseJSON = withObject "Meta" $ \o ->
        Meta
            <$> o .: "entityId"
            <*> o .: "bornAt"
            <*> o .: "diedAt"

data Timeline (k :: EntityKind) a = Timeline
    { timelineMeta :: Meta k
    , timelineDeltas :: InsOrdHashMap Tick [a]
    }
    deriving (Generic, Show, Eq)

instance Semigroup (Timeline k a) where
    Timeline meta1 deltas1 <> Timeline _meta2 deltas2 =
        Timeline meta1 (deltas1 <> deltas2)

instance (ToJSON a) => ToJSON (Timeline k a) where
    toJSON (Timeline meta deltas) = object ["meta" .= meta, "deltas" .= deltas]

instance (FromJSON a) => FromJSON (Timeline k a) where
    parseJSON = withObject "Timeline" $ \o ->
        Timeline <$> o .: "meta" <*> o .: "deltas"
