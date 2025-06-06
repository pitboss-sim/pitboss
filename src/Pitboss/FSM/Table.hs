{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Table where

import Pitboss.FSM.Types.Core

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
    TableFSM 'TClosed ->
    TableFSM 'TOpening
openTable TClosedFSM = TOpeningFSM

beginRound ::
    TableFSM 'TOpening ->
    TableFSM 'TRoundInProgress
beginRound TOpeningFSM = TRoundInProgressFSM

pauseForTIntermission ::
    TableFSM 'TRoundInProgress ->
    TableFSM 'TIntermission
pauseForTIntermission TRoundInProgressFSM = TIntermissionFSM

resumeRound ::
    TableFSM 'TIntermission ->
    TableFSM 'TRoundInProgress
resumeRound TIntermissionFSM = TRoundInProgressFSM

closeTable ::
    TableFSM 'TIntermission ->
    TableFSM 'TClosing
closeTable TIntermissionFSM = TClosingFSM

completeTClosing ::
    TableFSM 'TClosing ->
    TableFSM 'TClosed
completeTClosing TClosingFSM = TClosedFSM

interruptTable ::
    TInterruptReason ->
    TableFSM from ->
    TableFSM ('TInterrupted r)
interruptTable reason _ = TInterruptedFSM reason

resumeFromTInterrupt ::
    TableFSM ('TInterrupted r) ->
    TableFSM 'TIntermission
resumeFromTInterrupt (TInterruptedFSM _) = TIntermissionFSM
