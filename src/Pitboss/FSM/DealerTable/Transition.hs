{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.DealerTable.Transition where

import Pitboss.FSM.DealerTable.FSM
import Pitboss.FSM.DealerTable.Phase

type family ValidDealerTableTransition (from :: DealerTablePhase) (to :: DealerTablePhase) :: Bool where
    ValidDealerTableTransition 'DTOffDuty 'DTOnDuty = 'True
    ValidDealerTableTransition 'DTOnDuty 'DTPushing = 'True
    ValidDealerTableTransition 'DTPushing 'DTOnDuty = 'True
    ValidDealerTableTransition 'DTOnDuty ('DTTasking task) = 'True
    ValidDealerTableTransition ('DTTasking task) 'DTOnDuty = 'True
    ValidDealerTableTransition 'DTOnDuty 'DTLeaving = 'True
    ValidDealerTableTransition 'DTLeaving 'DTOffDuty = 'True
    ValidDealerTableTransition _ _ = 'False

goOnDuty ::
    (ValidDealerTableTransition 'DTOffDuty 'DTOnDuty ~ 'True) =>
    DealerTableFSM 'DTOffDuty ->
    DealerTableFSM 'DTOnDuty
goOnDuty DTOffDutyFSM = DTOnDutyFSM

beginPushing ::
    (ValidDealerTableTransition 'DTOnDuty 'DTPushing ~ 'True) =>
    DealerTableFSM 'DTOnDuty ->
    DealerTableFSM 'DTPushing
beginPushing DTOnDutyFSM = DTPushingFSM

finishPushing ::
    (ValidDealerTableTransition 'DTPushing 'DTOnDuty ~ 'True) =>
    DealerTableFSM 'DTPushing ->
    DealerTableFSM 'DTOnDuty
finishPushing DTPushingFSM = DTOnDutyFSM

assignTask ::
    (ValidDealerTableTransition 'DTOnDuty ('DTTasking task) ~ 'True) =>
    DealerTableFSM 'DTOnDuty ->
    DealerTask ->
    DealerTableFSM ('DTTasking task)
assignTask DTOnDutyFSM = DTTaskingFSM

completeTask ::
    (ValidDealerTableTransition ('DTTasking task) 'DTOnDuty ~ 'True) =>
    DealerTableFSM ('DTTasking task) ->
    DealerTableFSM 'DTOnDuty
completeTask (DTTaskingFSM _) = DTOnDutyFSM

beginLeaving ::
    (ValidDealerTableTransition 'DTOnDuty 'DTLeaving ~ 'True) =>
    DealerTableFSM 'DTOnDuty ->
    DealerTableFSM 'DTLeaving
beginLeaving DTOnDutyFSM = DTLeavingFSM

completeLeaving ::
    (ValidDealerTableTransition 'DTLeaving 'DTOffDuty ~ 'True) =>
    DealerTableFSM 'DTLeaving ->
    DealerTableFSM 'DTOffDuty
completeLeaving DTLeavingFSM = DTOffDutyFSM
