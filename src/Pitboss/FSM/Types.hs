module Pitboss.FSM.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data InterruptReason
    = AttendingToPlayer
    | PitIntervention
    | Banking
    | Environment
    deriving (Eq, Show, Generic)

data InsuranceOutcome = Lost | Paid | PaidEvenMoney
    deriving (Eq, Show, Generic)

instance ToJSON InterruptReason
instance FromJSON InterruptReason
instance ToJSON InsuranceOutcome
instance FromJSON InsuranceOutcome
