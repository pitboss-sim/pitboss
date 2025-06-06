{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Dealer where

import Pitboss.FSM.Types

type family ValidDealerTableTransition (from :: DealerTablePhase) (to :: DealerTablePhase) :: Bool where
    ValidDealerTableTransition 'DOffDuty 'DOnDuty = 'True
    ValidDealerTableTransition 'DOnDuty 'DPushing = 'True
    ValidDealerTableTransition 'DPushing 'DOnDuty = 'True
    ValidDealerTableTransition 'DOnDuty ('DTasking task) = 'True
    ValidDealerTableTransition ('DTasking task) 'DOnDuty = 'True
    ValidDealerTableTransition 'DOnDuty 'DLeaving = 'True
    ValidDealerTableTransition 'DLeaving 'DOffDuty = 'True
    ValidDealerTableTransition _ _ = 'False

goOnDuty ::
    DealerTableFSM 'DOffDuty ->
    DealerTableFSM 'DOnDuty
goOnDuty DOffDutyFSM = DOnDutyFSM

beginPushing ::
    DealerTableFSM 'DOnDuty ->
    DealerTableFSM 'DPushing
beginPushing DOnDutyFSM = DPushingFSM

finishPushing ::
    DealerTableFSM 'DPushing ->
    DealerTableFSM 'DOnDuty
finishPushing DPushingFSM = DOnDutyFSM

assignTask ::
    DealerTableFSM 'DOnDuty ->
    DealerTask ->
    DealerTableFSM ('DTasking task)
assignTask DOnDutyFSM = DTaskingFSM

completeTask ::
    DealerTableFSM ('DTasking task) ->
    DealerTableFSM 'DOnDuty
completeTask (DTaskingFSM _) = DOnDutyFSM

beginLeaving ::
    DealerTableFSM 'DOnDuty ->
    DealerTableFSM 'DLeaving
beginLeaving DOnDutyFSM = DLeavingFSM

completeLeaving ::
    DealerTableFSM 'DLeaving ->
    DealerTableFSM 'DOffDuty
completeLeaving DLeavingFSM = DOffDutyFSM
