{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.Offering.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering qualified as O
import Pitboss.Trace.Entity.Types

mkOfferingEntityAttrs :: O.Offering -> OfferingEntityAttrs
mkOfferingEntityAttrs = OfferingEntityAttrs

mkOfferingEntityModes :: OfferingEntityModes
mkOfferingEntityModes = OfferingEntityModes

mkOfferingEntityRels :: OfferingEntityRels
mkOfferingEntityRels = OfferingEntityRels []

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

instance ToJSON OfferingEntityAttrs

instance FromJSON OfferingEntityAttrs

instance ToJSON OfferingEntityModes

instance FromJSON OfferingEntityModes

instance ToJSON OfferingEntityRels

instance FromJSON OfferingEntityRels
