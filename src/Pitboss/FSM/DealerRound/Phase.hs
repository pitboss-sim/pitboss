{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerRound.Phase where

import Data.Aeson.Types
import GHC.Generics
import Pitboss.FSM.Types

data DealerRoundPhase
    = DRAwaiting
    | DRBets
    | DRDeal
    | DREarlySurrender
    | DRInsuranceDecision
    | DRInsuranceSettled
    | DRPeek
    | DRLateSurrender
    | DRPlayers
    | DRDealing
    | DRSettle
    | DRComplete
    | DRInterrupted InterruptReason
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundPhase
instance FromJSON DealerRoundPhase
