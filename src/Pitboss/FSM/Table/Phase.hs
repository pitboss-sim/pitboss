{-# LANGUAGE DeriveGeneric #-}

module Pitboss.FSM.Table.Phase where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data InterruptReason
    = AttendingToPlayer
    | PitIntervention
    | Banking
    | Environment
    deriving (Eq, Show, Generic)

data TablePhase
    = Closed
    | Opening
    | RoundInProgress
    | Intermission
    | Interrupted InterruptReason
    | Closing
    deriving (Eq, Show, Generic)

instance ToJSON InterruptReason
instance FromJSON InterruptReason
instance ToJSON TablePhase
instance FromJSON TablePhase
