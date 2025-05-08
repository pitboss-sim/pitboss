{-# LANGUAGE GADTs #-}

module Pitboss.FSM.PlayerTableFSM.Existential where

import Pitboss.FSM.PlayerTableFSM.Types

data SomePlayerTableFSM = forall p. SomePlayerTableFSM (PlayerTableFSM p)

instance Show SomePlayerTableFSM where
  show (SomePlayerTableFSM fsm) = show fsm

instance Eq SomePlayerTableFSM where
  SomePlayerTableFSM f1 == SomePlayerTableFSM f2 = case (f1, f2) of
    (IdleFSM, IdleFSM) -> True
    (ChoosingTableFSM, ChoosingTableFSM) -> True
    (PlacingBetFSM, PlacingBetFSM) -> True
    (PlayingHandFSM, PlayingHandFSM) -> True
    (ObservingFSM, ObservingFSM) -> True
    (DoneFSM, DoneFSM) -> True
    _ -> False
