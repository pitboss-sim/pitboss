{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.Offering.Entity where

import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering qualified as O

mkEOfferingAttrs :: O.Offering -> EOfferingAttrs
mkEOfferingAttrs = EOfferingAttrs

mkEOfferingModes :: EOfferingModes
mkEOfferingModes = EOfferingModes undefined

mkEOfferingRels :: EOfferingRels
mkEOfferingRels = EOfferingRels undefined

data EOfferingAttrs = EOfferingAttrs
    { _oAttrsOffering :: O.Offering
    }
    deriving (Eq, Show, Generic)

data EOfferingModes = EOfferingModes Void
    deriving (Eq, Show, Generic)

data EOfferingRels = EOfferingRels Void
    deriving (Eq, Show, Generic)

instance ToJSON EOfferingAttrs
instance FromJSON EOfferingAttrs

instance ToJSON EOfferingModes
instance FromJSON EOfferingModes

instance ToJSON EOfferingRels
instance FromJSON EOfferingRels
