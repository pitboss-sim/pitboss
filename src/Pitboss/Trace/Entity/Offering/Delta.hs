module Pitboss.Trace.Entity.Offering.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering as O
import Pitboss.Trace.Entity.Types.EntityId

data OfferingEntityAttrsDelta
    = ReplaceOffering O.Offering O.Offering
    deriving (Eq, Show, Generic)

instance ToJSON OfferingEntityAttrsDelta
instance FromJSON OfferingEntityAttrsDelta

data OfferingEntityModesDelta = NoopModes
    deriving (Eq, Show, Generic)

instance ToJSON OfferingEntityModesDelta
instance FromJSON OfferingEntityModesDelta

data OfferingEntityRelsDelta
    = AddTable TableEntityId
    | RemoveTable TableEntityId
    deriving (Eq, Show, Generic)

instance ToJSON OfferingEntityRelsDelta
instance FromJSON OfferingEntityRelsDelta
