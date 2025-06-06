{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Core.Player where

import Data.Aeson.Types
import GHC.Generics (Generic)

data PlayerPhase
    = PIdle
    | PChoosingTable
    | PPlacingBet
    | PPlayingHand
    | PObserving
    | PDone
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PlayerFSM (p :: PlayerPhase) where
    PIdleFSM :: PlayerFSM 'PIdle
    PChoosingTableFSM :: PlayerFSM 'PChoosingTable
    PPlacingBetFSM :: PlayerFSM 'PPlacingBet
    PPlayingHandFSM :: PlayerFSM 'PPlayingHand
    PObservingFSM :: PlayerFSM 'PObserving
    PDoneFSM :: PlayerFSM 'PDone

deriving instance Show (PlayerFSM p)
deriving instance Eq (PlayerFSM p)
