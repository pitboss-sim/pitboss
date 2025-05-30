{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.PlayerHand.FSM where

import Pitboss.FSM.PlayerHand.Phase
import Pitboss.FSM.Types.Transitionable

data PlayerHandFSM (p :: HandPhase) (h :: OHit) (d :: ODbl) (s :: OSpl) where
    PHAbandonedFSM :: AbandonedReason -> PlayerHandFSM ('PHAbandoned reason) 'NoHit 'NoDbl 'NoSpl
    PHBlackjackFSM :: PlayerHandFSM 'PHNaturalBlackjack 'NoHit 'NoDbl 'NoSpl
    PHDecisionFSM :: PlayerHandFSM 'PHDecision h d s
    PHHittingFSM :: PlayerHandFSM 'PHHitting h d s
    PHOneCardDrawFSM :: OneCardDrawReason -> PlayerHandFSM ('PHOneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
    PHResolvedFSM :: PlayerHandResolution -> PlayerHandFSM ('PHResolved res) 'NoHit 'NoDbl 'NoSpl

data OHit = OKHit | NoHit

data ODbl = OKDbl | NoDbl

data OSpl = OKSpl | NoSpl

deriving instance Show (PlayerHandFSM p h d s)

deriving instance Eq (PlayerHandFSM p h d s)

instance Transitionable (PlayerHandFSM p h d s) where
    transitionType = \case
        PHDecisionFSM -> AwaitInput
        PHHittingFSM -> AwaitInput
        PHOneCardDrawFSM _ -> AutoAdvance
        PHResolvedFSM _ -> TerminalPhase
        PHAbandonedFSM _ -> TerminalPhase
        PHBlackjackFSM -> TerminalPhase
