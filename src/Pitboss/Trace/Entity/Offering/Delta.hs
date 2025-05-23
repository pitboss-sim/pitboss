{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.Offering.Delta where

import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering as O

data DOfferingAttrs
    = DOfferingSetOffering O.Offering O.Offering
    deriving (Eq, Show, Generic)

instance ToJSON DOfferingAttrs
instance FromJSON DOfferingAttrs

data DOfferingModes = DOfferingModes Void
    deriving (Eq, Show, Generic)

instance ToJSON DOfferingModes
instance FromJSON DOfferingModes

data DOfferingRels = DOfferingRels Void
    deriving (Eq, Show, Generic)

instance ToJSON DOfferingRels
instance FromJSON DOfferingRels
