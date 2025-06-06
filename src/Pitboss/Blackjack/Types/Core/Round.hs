{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Types.Core.Round where

import Data.Aeson.Types
import GHC.Generics (Generic)

data InterruptReason
    = AttendingToPlayer
    | PitIntervention
    | Banking
    | Environment
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data RoundPhase
    = Awaiting
    | Bets
    | Deal
    | EarlySurrender
    | InsuranceDecision
    | InsuranceSettled
    | DealerPeek
    | LateSurrender
    | Players
    | Dealing
    | Settle
    | Complete
    | Interrupted InterruptReason
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
