{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.Offering where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering qualified as O
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.Identifier

mkOffering :: Meta OfferingEntityId -> OfferingEntityAttrs -> OfferingEntityModes -> OfferingEntityRels -> OfferingEntity
mkOffering = OfferingEntity

mkOfferingEntityAttrs :: O.Offering -> OfferingEntityAttrs
mkOfferingEntityAttrs = OfferingEntityAttrs

mkOfferingEntityModes :: OfferingEntityModes
mkOfferingEntityModes = OfferingEntityModes

mkOfferingEntityRels :: OfferingEntityRels
mkOfferingEntityRels = OfferingEntityRels []

data OfferingEntity = OfferingEntity
    { _offeringEntityMeta :: Meta OfferingEntityId
    , _offeringEntityAttrs :: OfferingEntityAttrs
    , _offeringEntityModes :: OfferingEntityModes
    , _offeringEntityRels :: OfferingEntityRels
    }
    deriving (Eq, Show, Generic)

data OfferingEntityAttrs = OfferingEntityAttrs
    { _offeringEntityAttrsOffering :: O.Offering
    }
    deriving (Eq, Show, Generic)

data OfferingEntityModes = OfferingEntityModes
    {
    }
    deriving (Eq, Show, Generic)

data OfferingEntityRels = OfferingEntityRels
    { _offeringEntityRelsAssociatedTables :: [TableEntityId]
    }
    deriving (Eq, Show, Generic)

instance ToJSON OfferingEntity

instance FromJSON OfferingEntity

instance ToJSON OfferingEntityAttrs

instance FromJSON OfferingEntityAttrs

instance ToJSON OfferingEntityModes

instance FromJSON OfferingEntityModes

instance ToJSON OfferingEntityRels

instance FromJSON OfferingEntityRels
