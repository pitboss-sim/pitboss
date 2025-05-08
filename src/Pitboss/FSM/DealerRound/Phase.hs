{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerRound.Phase where

import Data.Aeson.Types
import GHC.Generics

data InterruptReason
    = AttendingToPlayer
    | PitIntervention
    | Banking
    | Environment
    deriving (Eq, Show, Generic)

data DealerRoundPhase
    = Awaiting
    | Bets
    | Deal
    | EarlySurrender
    | InsuranceDecision
    | InsuranceSettled
    | Peek
    | LateSurrender
    | Players
    | Dealer
    | Settle
    | Complete
    | Interrupted InterruptReason
    deriving (Eq, Show, Generic)

instance ToJSON InterruptReason

instance FromJSON InterruptReason

instance ToJSON DealerRoundPhase

instance FromJSON DealerRoundPhase
