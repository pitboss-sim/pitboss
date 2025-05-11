{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.Offering where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Offering.Types
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.Identifier

mkOffering :: Meta OfferingEntityId -> OfferingEntityAttrs -> OfferingEntityModes -> OfferingEntityRels -> OfferingEntity
mkOffering = OfferingEntity

data OfferingEntity = OfferingEntity
    { _offeringEntityMeta :: Meta OfferingEntityId
    , _offeringEntityAttrs :: OfferingEntityAttrs
    , _offeringEntityModes :: OfferingEntityModes
    , _offeringEntityRels :: OfferingEntityRels
    }
    deriving (Eq, Show, Generic)

instance ToJSON OfferingEntity

instance FromJSON OfferingEntity
