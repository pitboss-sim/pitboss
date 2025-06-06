module Pitboss.Blackjack.Types.Core.Decision where

import Data.Aeson.Types
import GHC.Generics (Generic)

data Move = MHit | MStand | MDouble | MSplit | MSurrender
    deriving (Eq, Show, Generic)

data InsuranceDecision = TakeInsurance | DeclineInsurance
    deriving (Eq, Show, Generic)

data Surrender = Early | Late | NoSurrender
    deriving (Show, Eq, Generic)

instance ToJSON Move
instance FromJSON Move

instance ToJSON InsuranceDecision
instance FromJSON InsuranceDecision

instance ToJSON Surrender
instance FromJSON Surrender
