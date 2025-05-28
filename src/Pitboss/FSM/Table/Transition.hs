{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.Table.Transition where

import Pitboss.FSM.Table.FSM
import Pitboss.FSM.Table.Phase

type family ValidTableTransition (from :: TablePhase) (to :: TablePhase) :: Bool where
    ValidTableTransition 'Closed 'Opening = 'True
    ValidTableTransition 'Opening 'RoundInProgress = 'True
    ValidTableTransition 'RoundInProgress 'Intermission = 'True
    ValidTableTransition 'Intermission 'RoundInProgress = 'True
    ValidTableTransition 'Intermission 'Closing = 'True
    ValidTableTransition 'Closing 'Closed = 'True
    ValidTableTransition p ('Interrupted r) = 'True
    ValidTableTransition ('Interrupted r) 'Intermission = 'True
    ValidTableTransition _ _ = 'False

openTable ::
    (ValidTableTransition 'Closed 'Opening ~ 'True) =>
    TableFSM 'Closed ->
    TableFSM 'Opening
openTable ClosedFSM = OpeningFSM

beginRound ::
    (ValidTableTransition 'Opening 'RoundInProgress ~ 'True) =>
    TableFSM 'Opening ->
    TableFSM 'RoundInProgress
beginRound OpeningFSM = RoundInProgressFSM

pauseForIntermission ::
    (ValidTableTransition 'RoundInProgress 'Intermission ~ 'True) =>
    TableFSM 'RoundInProgress ->
    TableFSM 'Intermission
pauseForIntermission RoundInProgressFSM = IntermissionFSM

resumeRound ::
    (ValidTableTransition 'Intermission 'RoundInProgress ~ 'True) =>
    TableFSM 'Intermission ->
    TableFSM 'RoundInProgress
resumeRound IntermissionFSM = RoundInProgressFSM

closeTable ::
    (ValidTableTransition 'Intermission 'Closing ~ 'True) =>
    TableFSM 'Intermission ->
    TableFSM 'Closing
closeTable IntermissionFSM = ClosingFSM

completeClosing ::
    (ValidTableTransition 'Closing 'Closed ~ 'True) =>
    TableFSM 'Closing ->
    TableFSM 'Closed
completeClosing ClosingFSM = ClosedFSM

interruptTable ::
    (ValidTableTransition from ('Interrupted r) ~ 'True) =>
    InterruptReason ->
    TableFSM from ->
    TableFSM ('Interrupted r)
interruptTable reason _ = InterruptedFSM reason

resumeFromInterrupt ::
    (ValidTableTransition ('Interrupted r) 'Intermission ~ 'True) =>
    TableFSM ('Interrupted r) ->
    TableFSM 'Intermission
resumeFromInterrupt (InterruptedFSM _) = IntermissionFSM
