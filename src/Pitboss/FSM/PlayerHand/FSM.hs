{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.PlayerHand.FSM where

import Pitboss.FSM.PlayerHand.Phase
import Pitboss.FSM.Types.Transitionable

data PlayerHandFSM (p :: HandPhase) (h :: OHit) (d :: ODbl) (s :: OSpl) where
    AbandonedFSM :: AbandonedReason -> PlayerHandFSM ('Abandoned reason) 'NoHit 'NoDbl 'NoSpl
    BlackjackFSM :: PlayerHandFSM 'NaturalBlackjack 'NoHit 'NoDbl 'NoSpl
    DecisionFSM :: PlayerHandFSM 'Decision h d s
    HittingFSM :: PlayerHandFSM 'Hitting h d s
    OneCardDrawFSM :: OneCardDrawReason -> PlayerHandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
    ResolvedFSM :: PlayerHandResolution -> PlayerHandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl

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
