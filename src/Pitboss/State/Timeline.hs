{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.State.Timeline (
    Timeline (..),
    Meta,
    mkTimeline,
) where

import Data.Aeson
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol)
import Pitboss.State.Entity.Types
import Pitboss.State.Types.Core
import Prelude hiding (id)

mkTimeline :: Uid k -> Tick -> Timeline k a
mkTimeline id birthTick =
    Timeline
        { timelineMeta = Meta id birthTick Nothing
        , timelineDeltas = mempty
        }

data family Meta (k :: EntityKind)

data instance Meta k = Meta
    { _metaId :: Uid k
    , _metaBornAt :: Tick
    , _metaDiedAt :: Maybe Tick -- temporal bigotry :(
    }
    deriving (Generic, Show, Eq)

instance (KnownSymbol (UidPrefix k)) => ToJSON (Meta k) where
    toJSON (Meta uid born died) =
        object
            [ "uid" .= uid
            , "bornAt" .= born
            , "diedAt" .= died
            ]

instance (KnownSymbol (UidPrefix k)) => FromJSON (Meta k) where
    parseJSON = withObject "Meta" $ \o ->
        Meta
            <$> o .: "uid"
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

instance (ToJSON a, KnownSymbol (UidPrefix k)) => ToJSON (Timeline k a) where
    toJSON (Timeline meta deltas) = object ["meta" .= meta, "deltas" .= deltas]

instance (FromJSON a, KnownSymbol (UidPrefix k)) => FromJSON (Timeline k a) where
    parseJSON = withObject "Timeline" $ \o ->
        Timeline <$> o .: "meta" <*> o .: "deltas"
