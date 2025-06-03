{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.Causality.Timeline (
    Timeline (..),
    Meta (..),
    mkTimeline,
) where

import Data.Aeson
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import GHC.Generics (Generic)
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Types.Core
import Prelude hiding (id)

mkTimeline :: EntityId k -> Tick -> EntityState k -> Timeline k a
mkTimeline entityId birthTick initialState =
    Timeline
        { timelineMeta = Meta entityId birthTick Nothing
        , timelineInitialState = Just initialState
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
    , timelineInitialState :: Maybe (EntityState k)
    , timelineDeltas :: InsOrdHashMap Tick [a]
    }

deriving instance (Eq a, Eq (EntityState k)) => Eq (Timeline k a)
deriving instance (Show a, Show (EntityState k)) => Show (Timeline k a)

instance Semigroup (Timeline k a) where
    Timeline meta1 initialState1 deltas1 <> Timeline _meta2 _initiatState2 deltas2 =
        Timeline meta1 initialState1 (deltas1 <> deltas2)

instance (ToJSON a, ToJSON (EntityState k)) => ToJSON (Timeline k a) where
    toJSON (Timeline meta initialState deltas) = object ["meta" .= meta, "initialState" .= initialState, "deltas" .= deltas]

instance (FromJSON a, FromJSON (EntityState k)) => FromJSON (Timeline k a) where
    parseJSON = withObject "Timeline" $ \o ->
        Timeline <$> o .: "meta" <*> o .: "initialState" <*> o .: "deltas"
