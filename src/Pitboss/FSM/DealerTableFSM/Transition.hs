{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerTableFSM.Transition where

import Pitboss.FSM.DealerTableFSM.FSM
import Pitboss.FSM.DealerTableFSM.Phase

goOnDuty :: DealerTableFSM 'OffDuty -> DealerTableFSM 'OnDuty
goOnDuty OffDutyFSM = OnDutyFSM

beginPushing :: DealerTableFSM 'OnDuty -> DealerTableFSM 'Pushing
beginPushing OnDutyFSM = PushingFSM

finishPushing :: DealerTableFSM 'Pushing -> DealerTableFSM 'OnDuty
finishPushing PushingFSM = OnDutyFSM

assignTask :: DealerTableFSM 'OnDuty -> DealerTask -> DealerTableFSM ('Tasking task)
assignTask OnDutyFSM = TaskingFSM

completeTask :: DealerTableFSM ('Tasking task) -> DealerTableFSM 'OnDuty
completeTask (TaskingFSM _) = OnDutyFSM

beginLeaving :: DealerTableFSM 'OnDuty -> DealerTableFSM 'Leaving
beginLeaving OnDutyFSM = LeavingFSM

completeLeaving :: DealerTableFSM 'Leaving -> DealerTableFSM 'OffDuty
completeLeaving LeavingFSM = OffDutyFSM
