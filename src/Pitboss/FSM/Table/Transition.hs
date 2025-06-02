{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.Table.Transition where

import Pitboss.FSM.Table.FSM
import Pitboss.FSM.Table.Phase

type family ValidTableTransition (from :: TablePhase) (to :: TablePhase) :: Bool where
    ValidTableTransition 'TClosed 'TOpening = 'True
    ValidTableTransition 'TOpening 'TRoundInProgress = 'True
    ValidTableTransition 'TRoundInProgress 'TIntermission = 'True
    ValidTableTransition 'TIntermission 'TRoundInProgress = 'True
    ValidTableTransition 'TIntermission 'TClosing = 'True
    ValidTableTransition 'TClosing 'TClosed = 'True
    ValidTableTransition p ('TInterrupted r) = 'True
    ValidTableTransition ('TInterrupted r) 'TIntermission = 'True
    ValidTableTransition _ _ = 'False

openTable ::
    (ValidTableTransition 'TClosed 'TOpening ~ 'True) =>
    TableFSM 'TClosed ->
    TableFSM 'TOpening
openTable TClosedFSM = TOpeningFSM

beginRound ::
    (ValidTableTransition 'TOpening 'TRoundInProgress ~ 'True) =>
    TableFSM 'TOpening ->
    TableFSM 'TRoundInProgress
beginRound TOpeningFSM = TRoundInProgressFSM

pauseForTIntermission ::
    (ValidTableTransition 'TRoundInProgress 'TIntermission ~ 'True) =>
    TableFSM 'TRoundInProgress ->
    TableFSM 'TIntermission
pauseForTIntermission TRoundInProgressFSM = TIntermissionFSM

resumeRound ::
    (ValidTableTransition 'TIntermission 'TRoundInProgress ~ 'True) =>
    TableFSM 'TIntermission ->
    TableFSM 'TRoundInProgress
resumeRound TIntermissionFSM = TRoundInProgressFSM

closeTable ::
    (ValidTableTransition 'TIntermission 'TClosing ~ 'True) =>
    TableFSM 'TIntermission ->
    TableFSM 'TClosing
closeTable TIntermissionFSM = TClosingFSM

completeTClosing ::
    (ValidTableTransition 'TClosing 'TClosed ~ 'True) =>
    TableFSM 'TClosing ->
    TableFSM 'TClosed
completeTClosing TClosingFSM = TClosedFSM

interruptTable ::
    (ValidTableTransition from ('TInterrupted r) ~ 'True) =>
    TInterruptReason ->
    TableFSM from ->
    TableFSM ('TInterrupted r)
interruptTable reason _ = TInterruptedFSM reason

resumeFromTInterrupt ::
    (ValidTableTransition ('TInterrupted r) 'TIntermission ~ 'True) =>
    TableFSM ('TInterrupted r) ->
    TableFSM 'TIntermission
resumeFromTInterrupt (TInterruptedFSM _) = TIntermissionFSM
