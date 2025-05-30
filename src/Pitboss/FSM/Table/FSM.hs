{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.Table.FSM where

import Pitboss.FSM.Table.Phase
import Pitboss.FSM.Types.Transitionable

data TableFSM (p :: TablePhase) where
    TClosedFSM :: TableFSM 'TClosed
    TOpeningFSM :: TableFSM 'TOpening
    TRoundInProgressFSM :: TableFSM 'TRoundInProgress
    TIntermissionFSM :: TableFSM 'TIntermission
    TInterruptedFSM :: TInterruptReason -> TableFSM ('TInterrupted r)
    TClosingFSM :: TableFSM 'TClosing

deriving instance Show (TableFSM p)
deriving instance Eq (TableFSM p)

instance Transitionable (TableFSM p) where
    transitionType = \case
        TClosedFSM -> AwaitInput
        TOpeningFSM -> AutoAdvance
        TRoundInProgressFSM -> AwaitInput
        TIntermissionFSM -> AutoAdvance
        TInterruptedFSM _ -> AwaitInput
        TClosingFSM -> AutoAdvance
