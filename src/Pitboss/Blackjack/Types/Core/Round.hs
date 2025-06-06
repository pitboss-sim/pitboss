module Pitboss.Blackjack.Types.Core.Round where

import Data.Aeson.Types
import GHC.Generics (Generic)

data InterruptReason
    = AttendingToContestant
    | PitIntervention
    | Banking
    | Environment
    deriving (Eq, Show, Generic)

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
    deriving (Eq, Show, Generic)

instance ToJSON InterruptReason
instance FromJSON InterruptReason

instance ToJSON RoundPhase
instance FromJSON RoundPhase
