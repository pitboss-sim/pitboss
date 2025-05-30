{-# LANGUAGE DeriveGeneric #-}

module Pitboss.FSM.Table.Phase where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data TInterruptReason
    = TAttendingToPlayer
    | TPitIntervention
    | TBanking
    | TEnvironment
    deriving (Eq, Show, Generic)

data TablePhase
    = TClosed
    | TOpening
    | TRoundInProgress
    | TIntermission
    | TInterrupted TInterruptReason
    | TClosing
    deriving (Eq, Show, Generic)

instance ToJSON TInterruptReason
instance FromJSON TInterruptReason
instance ToJSON TablePhase
instance FromJSON TablePhase
