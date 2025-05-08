{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.Table.Transition where

import Pitboss.FSM.Table.FSM
import Pitboss.FSM.Table.Phase

openTable :: TableFSM 'Closed -> TableFSM 'Opening
openTable ClosedFSM = OpeningFSM

beginRound :: TableFSM 'Opening -> TableFSM 'RoundInProgress
beginRound OpeningFSM = RoundInProgressFSM

pauseForIntermission :: TableFSM 'RoundInProgress -> TableFSM 'Intermission
pauseForIntermission RoundInProgressFSM = IntermissionFSM

resumeRound :: TableFSM 'Intermission -> TableFSM 'RoundInProgress
resumeRound IntermissionFSM = RoundInProgressFSM

closeTable :: TableFSM 'Intermission -> TableFSM 'Closing
closeTable IntermissionFSM = ClosingFSM

completeClosing :: TableFSM 'Closing -> TableFSM 'Closed
completeClosing ClosingFSM = ClosedFSM

interruptTable :: InterruptReason -> TableFSM p -> TableFSM ('Interrupted r)
interruptTable reason _ = InterruptedFSM reason

resumeFromInterrupt :: TableFSM ('Interrupted r) -> TableFSM 'Intermission
resumeFromInterrupt (InterruptedFSM _) = IntermissionFSM
