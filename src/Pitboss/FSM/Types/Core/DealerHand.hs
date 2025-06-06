{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.Types.Core.DealerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types

data DealerHandPhase
    = DHAwaitingFirstCard
    | DHAwaitingSecondCard
    | DHDealing
    | DHEvaluating
    | DHPlaying
    | DHResolved DealerHandResolution
    | DHInterrupted InterruptReason
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DealerHandFSM (p :: DealerHandPhase) where
    DHAwaitingFirstCardFSM :: DealerHandFSM 'DHAwaitingFirstCard
    DHAwaitingSecondCardFSM :: DealerHandFSM 'DHAwaitingSecondCard
    DHDealingFSM :: DealerHandFSM 'DHDealing
    DHEvaluatingFSM :: DealerHandFSM 'DHEvaluating
    DHPlayingFSM :: DealerHandFSM 'DHPlaying
    DHResolvedFSM :: DealerHandResolution -> DealerHandFSM ('DHResolved res)
    DHInterruptedFSM :: InterruptReason -> DealerHandFSM ('DHInterrupted reason)

deriving instance Show (DealerHandFSM p)
deriving instance Eq (DealerHandFSM p)
