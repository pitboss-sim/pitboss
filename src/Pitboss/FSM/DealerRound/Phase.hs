{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerRound.Phase where

import Data.Aeson.Types
import GHC.Generics
import Pitboss.FSM.Types

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
    | Dealing
    | Settle
    | Complete
    | Interrupted InterruptReason
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundPhase

instance FromJSON DealerRoundPhase
