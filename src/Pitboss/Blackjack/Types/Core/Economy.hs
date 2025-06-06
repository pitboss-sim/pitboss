module Pitboss.Blackjack.Types.Core.Economy where

import Data.Aeson.Types
import GHC.Generics (Generic)

newtype Chips = Chips Int
    deriving (Eq, Ord, Show, Num, Generic)

data BankrollImpact = Loss | Refund
    deriving (Eq, Show, Generic)

data InsuranceOutcome = Lost | Paid | PaidEvenMoney
    deriving (Eq, Show, Generic)

instance ToJSON Chips
instance FromJSON Chips

instance ToJSON BankrollImpact
instance FromJSON BankrollImpact

instance ToJSON InsuranceOutcome
instance FromJSON InsuranceOutcome
