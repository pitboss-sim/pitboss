{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.Types.Core.PlayerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types

data PlayerHandPhase
    = PHAwaitingFirstCard
    | PHAwaitingSecondCard
    | PHDecision
    | PHHitting
    | PHAwaitingOneCard OneCardDrawReason
    | PHResolved PlayerHandResolution
    | PHAbandoned AbandonedReason
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data OHit = OKHit | NoHit deriving (Eq, Show)
data ODbl = OKDbl | NoDbl deriving (Eq, Show)
data OSpl = OKSpl | NoSpl deriving (Eq, Show)

data PlayerHandFSM (p :: PlayerHandPhase) (h :: OHit) (d :: ODbl) (s :: OSpl) where
    PHAwaitingFirstCardFSM :: PlayerHandFSM 'PHAwaitingFirstCard 'NoHit 'NoDbl 'NoSpl
    PHAwaitingSecondCardFSM :: PlayerHandFSM 'PHAwaitingSecondCard 'NoHit 'NoDbl 'NoSpl
    PHDecisionFSM :: PlayerHandFSM 'PHDecision h d s
    PHHittingFSM :: PlayerHandFSM 'PHHitting h d s
    PHAwaitingOneCardFSM :: OneCardDrawReason -> PlayerHandFSM ('PHAwaitingOneCard reason) 'NoHit 'NoDbl 'NoSpl
    PHResolvedFSM :: PlayerHandResolution -> PlayerHandFSM ('PHResolved res) 'NoHit 'NoDbl 'NoSpl
    PHAbandonedFSM :: AbandonedReason -> PlayerHandFSM ('PHAbandoned reason) 'NoHit 'NoDbl 'NoSpl

deriving instance Show (PlayerHandFSM p h d s)
deriving instance Eq (PlayerHandFSM p h d s)
