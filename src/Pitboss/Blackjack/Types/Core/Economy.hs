{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Types.Core.Economy where

import Data.Aeson.Types
import GHC.Generics (Generic)

newtype Chips = Chips Int
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Num Chips where
    Chips a + Chips b = Chips (a + b)
    Chips a - Chips b = Chips (a - b)
    Chips a * Chips b = Chips (a * b)
    abs (Chips a) = Chips (abs a)
    signum (Chips a) = Chips (signum a)
    fromInteger = Chips . fromInteger

data BankrollImpact = Loss | Refund
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data InsuranceOutcome = Lost | Paid | PaidEvenMoney
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
