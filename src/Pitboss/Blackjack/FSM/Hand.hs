{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Blackjack.FSM.Hand where

data HandPhase
  = PreDeal
  | Dealt1
  | Dealt2
  | AwaitingDecision
  | Hitting
  | Doubling
  | SplitAcesAuto
  | Completed

data DoublePotential
  = DoubleOK
  | NoDouble

data SplitPotential
  = SplitOK
  | NoSplit

data SurrenderPotential
  = SurrenderOK
  | NoSurrender

data HandTerminus
  = Stand
  | Bust
  | Surrender
  | Blackjack
  | SplitFinal
  | Double
  | Forfeit
  deriving (Eq, Show)

data
  HandFSM
    (p :: HandPhase)
    (s :: SplitPotential)
    (d :: DoublePotential)
    (r :: SurrenderPotential)
  where
  PreDealFSM :: HandFSM 'PreDeal s d r
  Dealt1FSM :: HandFSM 'Dealt1 s d r
  Dealt2FSM :: HandFSM 'Dealt2 s d r
  AwaitingDecisionFSM :: HandFSM 'AwaitingDecision s d r
  HittingFSM :: HandFSM 'Hitting s d r
  DoublingFSM :: HandFSM 'Doubling 'NoSplit 'DoubleOK r
  SplitAcesAutoFSM :: HandFSM 'SplitAcesAuto 'NoSplit d r
  CompletedFSM :: HandTerminus -> HandFSM 'Completed s d r

deriving instance Show (HandFSM p s d r)

data SomeHandFSM where
  SomeHandFSM :: HandFSM p s d r -> SomeHandFSM

-- advance

dealFirstCard ::
  HandFSM 'PreDeal s d r ->
  HandFSM 'Dealt1 s d r
dealFirstCard PreDealFSM = Dealt1FSM

dealSecondCard ::
  HandFSM 'Dealt1 s d r ->
  HandFSM 'Dealt2 s d r
dealSecondCard Dealt1FSM = Dealt2FSM

pauseForDecision ::
  HandFSM 'Dealt2 s d r ->
  HandFSM 'AwaitingDecision s d r
pauseForDecision Dealt2FSM = AwaitingDecisionFSM

beginHitting ::
  HandFSM 'AwaitingDecision s d r ->
  HandFSM 'Hitting s d r
beginHitting AwaitingDecisionFSM = HittingFSM

completeFromHitting ::
  HandTerminus ->
  HandFSM 'Hitting s d r ->
  HandFSM 'Completed s d r
completeFromHitting reason _ = CompletedFSM reason

beginDouble ::
  HandFSM 'AwaitingDecision 'NoSplit 'DoubleOK r ->
  HandFSM 'Doubling 'NoSplit 'DoubleOK r
beginDouble AwaitingDecisionFSM = DoublingFSM

completeDouble ::
  HandFSM 'Doubling 'NoSplit 'DoubleOK r ->
  HandFSM 'Completed 'NoSplit 'DoubleOK r
completeDouble DoublingFSM = CompletedFSM Double

-- | Split Aces variant: only one card, no further play.
beginSplitAcesAuto ::
  HandFSM 'AwaitingDecision 'SplitOK d r ->
  HandFSM 'SplitAcesAuto 'NoSplit d r
beginSplitAcesAuto _ = SplitAcesAutoFSM

-- | One-card draw after split Aces.
completeSplitAces ::
  HandTerminus ->
  HandFSM 'SplitAcesAuto 'NoSplit d r ->
  HandFSM 'Completed 'NoSplit d r
completeSplitAces term _ = CompletedFSM term
