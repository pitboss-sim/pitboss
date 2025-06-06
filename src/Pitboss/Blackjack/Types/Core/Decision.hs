{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Types.Core.Decision where

import Data.Aeson.Types
import GHC.Generics (Generic)

data Move = MHit | MStand | MDouble | MSplit | MSurrender
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data InsuranceDecision = TakeInsurance | DeclineInsurance
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Surrender = Early | Late | NoSurrender
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
