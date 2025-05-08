{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Mechanics.Player.Types where

import Pitboss.Blackjack.Offering.RuleSet (InsuranceOutcome (..), Surrender (..))
import Pitboss.Mechanics.Types.Transitionable

-- shared types

data AbandonedReason
  = Surrender Surrender
  | Insurance InsuranceOutcome
  deriving (Eq, Show)

data HandPhase
  = Abandoned AbandonedReason
  | NaturalBlackjack
  | Decision
  | Hitting
  | OneCardDraw OneCardDrawReason
  | Resolved HandResolution
  deriving (Eq, Show)

data HandResolution
  = Surrendered
  | Blackjack
  | Stand
  | Bust
  | Push
  | SplitNonAces
  | SplitAces
  | DealerBlackjack
  | Void BankrollImpact
  deriving (Eq, Show)

data OneCardDrawReason = Double | SplitAce
  deriving (Eq, Show)

data BankrollImpact = Loss | Refund
  deriving (Eq, Show)

data OHit = OKHit | NoHit

data ODbl = OKDbl | NoDbl

data OSpl = OKSpl | NoSpl

-- player fsm
-- TBD

-- player hand fsm

data PlayerHandFSM (p :: HandPhase) (h :: OHit) (d :: ODbl) (s :: OSpl) where
  AbandonedFSM :: AbandonedReason -> PlayerHandFSM ('Abandoned reason) 'NoHit 'NoDbl 'NoSpl
  BlackjackFSM :: PlayerHandFSM 'NaturalBlackjack 'NoHit 'NoDbl 'NoSpl
  DecisionFSM :: PlayerHandFSM 'Decision h d s
  HittingFSM :: PlayerHandFSM 'Hitting h d s
  OneCardDrawFSM :: OneCardDrawReason -> PlayerHandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
  ResolvedFSM :: HandResolution -> PlayerHandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl

deriving instance Show (PlayerHandFSM p h d s)

deriving instance Eq (PlayerHandFSM p h d s)

data SomePlayerHandFSM = forall p h d s. SomePlayerHandFSM (PlayerHandFSM p h d s)

mkPlayerHandFSMAbandoned :: AbandonedReason -> SomePlayerHandFSM
mkPlayerHandFSMAbandoned reason = SomePlayerHandFSM (AbandonedFSM reason)

-- transitionable instances

instance Transitionable (PlayerHandFSM p h d s) where
  transitionType = \case
    DecisionFSM -> AwaitInput
    HittingFSM -> AwaitInput
    OneCardDrawFSM _ -> AutoAdvance
    ResolvedFSM _ -> TerminalPhase
    AbandonedFSM _ -> TerminalPhase
    BlackjackFSM -> TerminalPhase
