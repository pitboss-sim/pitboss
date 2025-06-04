module Pitboss.Blackjack.Actions where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Move = Hit | Stand | Double | Split | Surrender
    deriving (Eq, Show, Generic)

data InsuranceDecision = TakeInsurance | DeclineInsurance
    deriving (Eq, Show, Generic)

instance ToJSON Move
instance FromJSON Move

instance ToJSON InsuranceDecision
instance FromJSON InsuranceDecision
