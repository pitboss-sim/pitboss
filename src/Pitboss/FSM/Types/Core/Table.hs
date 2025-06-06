{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Core.Table where

import Data.Aeson.Types
import GHC.Generics (Generic)

data TInterruptReason
    = TAttendingToPlayer
    | TPitIntervention
    | TBanking
    | TEnvironment
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data TablePhase
    = TClosed
    | TOpening
    | TRoundInProgress
    | TIntermission
    | TInterrupted TInterruptReason
    | TClosing
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data TableFSM (p :: TablePhase) where
    TClosedFSM :: TableFSM 'TClosed
    TOpeningFSM :: TableFSM 'TOpening
    TRoundInProgressFSM :: TableFSM 'TRoundInProgress
    TIntermissionFSM :: TableFSM 'TIntermission
    TInterruptedFSM :: TInterruptReason -> TableFSM ('TInterrupted r)
    TClosingFSM :: TableFSM 'TClosing

deriving instance Show (TableFSM p)
deriving instance Eq (TableFSM p)
