{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.DealerTable.Transition where

import Pitboss.FSM.DealerTable.FSM
import Pitboss.FSM.DealerTable.Phase

type family ValidDealerTableTransition (from :: DealerTablePhase) (to :: DealerTablePhase) :: Bool where
    ValidDealerTableTransition 'OffDuty 'OnDuty = 'True
    ValidDealerTableTransition 'OnDuty 'Pushing = 'True
    ValidDealerTableTransition 'Pushing 'OnDuty = 'True
    ValidDealerTableTransition 'OnDuty ('Tasking task) = 'True
    ValidDealerTableTransition ('Tasking task) 'OnDuty = 'True
    ValidDealerTableTransition 'OnDuty 'Leaving = 'True
    ValidDealerTableTransition 'Leaving 'OffDuty = 'True
    ValidDealerTableTransition _ _ = 'False

goOnDuty ::
    (ValidDealerTableTransition 'OffDuty 'OnDuty ~ 'True) =>
    DealerTableFSM 'OffDuty ->
    DealerTableFSM 'OnDuty
goOnDuty OffDutyFSM = OnDutyFSM

beginPushing ::
    (ValidDealerTableTransition 'OnDuty 'Pushing ~ 'True) =>
    DealerTableFSM 'OnDuty ->
    DealerTableFSM 'Pushing
beginPushing OnDutyFSM = PushingFSM

finishPushing ::
    (ValidDealerTableTransition 'Pushing 'OnDuty ~ 'True) =>
    DealerTableFSM 'Pushing ->
    DealerTableFSM 'OnDuty
finishPushing PushingFSM = OnDutyFSM

assignTask ::
    (ValidDealerTableTransition 'OnDuty ('Tasking task) ~ 'True) =>
    DealerTableFSM 'OnDuty ->
    DealerTask ->
    DealerTableFSM ('Tasking task)
assignTask OnDutyFSM = TaskingFSM

completeTask ::
    (ValidDealerTableTransition ('Tasking task) 'OnDuty ~ 'True) =>
    DealerTableFSM ('Tasking task) ->
    DealerTableFSM 'OnDuty
completeTask (TaskingFSM _) = OnDutyFSM

beginLeaving ::
    (ValidDealerTableTransition 'OnDuty 'Leaving ~ 'True) =>
    DealerTableFSM 'OnDuty ->
    DealerTableFSM 'Leaving
beginLeaving OnDutyFSM = LeavingFSM

completeLeaving ::
    (ValidDealerTableTransition 'Leaving 'OffDuty ~ 'True) =>
    DealerTableFSM 'Leaving ->
    DealerTableFSM 'OffDuty
completeLeaving LeavingFSM = OffDutyFSM
