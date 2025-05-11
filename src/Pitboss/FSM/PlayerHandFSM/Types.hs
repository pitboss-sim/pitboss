{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.PlayerHandFSM.Types where

import Pitboss.Blackjack.Offering.RuleSet
import Pitboss.FSM.Types.Transitionable

data PlayerHandFSM (p :: HandPhase) (h :: OHit) (d :: ODbl) (s :: OSpl) where
  AbandonedFSM :: AbandonedReason -> PlayerHandFSM ('Abandoned reason) 'NoHit 'NoDbl 'NoSpl
  BlackjackFSM :: PlayerHandFSM 'NaturalBlackjack 'NoHit 'NoDbl 'NoSpl
  DecisionFSM :: PlayerHandFSM 'Decision h d s
  HittingFSM :: PlayerHandFSM 'Hitting h d s
  OneCardDrawFSM :: OneCardDrawReason -> PlayerHandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
  ResolvedFSM :: HandResolution -> PlayerHandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl

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

deriving instance Show (PlayerHandFSM p h d s)

deriving instance Eq (PlayerHandFSM p h d s)

instance Transitionable (PlayerHandFSM p h d s) where
  transitionType = \case
    DecisionFSM -> AwaitInput
    HittingFSM -> AwaitInput
    OneCardDrawFSM _ -> AutoAdvance
    ResolvedFSM _ -> TerminalPhase
    AbandonedFSM _ -> TerminalPhase
    BlackjackFSM -> TerminalPhase
