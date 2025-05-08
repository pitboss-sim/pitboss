{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.Table.FSM where

import Pitboss.FSM.Table.Phase
import Pitboss.FSM.Types.Transitionable

data TableFSM (p :: TablePhase) where
    ClosedFSM :: TableFSM 'Closed
    OpeningFSM :: TableFSM 'Opening
    RoundInProgressFSM :: TableFSM 'RoundInProgress
    IntermissionFSM :: TableFSM 'Intermission
    InterruptedFSM :: InterruptReason -> TableFSM ('Interrupted r)
    ClosingFSM :: TableFSM 'Closing

deriving instance Show (TableFSM p)
deriving instance Eq (TableFSM p)

instance Transitionable (TableFSM p) where
    transitionType = \case
        ClosedFSM -> AwaitInput
        OpeningFSM -> AutoAdvance
        RoundInProgressFSM -> AwaitInput
        IntermissionFSM -> AutoAdvance
        InterruptedFSM _ -> AwaitInput
        ClosingFSM -> AutoAdvance
