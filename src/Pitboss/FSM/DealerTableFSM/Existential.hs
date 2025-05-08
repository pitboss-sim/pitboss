{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerTableFSM.Existential where

import Pitboss.FSM.DealerTableFSM.Types

data SomeDealerTableFSM = forall p. SomeDealerTableFSM (DealerTableFSM p)

instance Eq SomeDealerTableFSM where
  (SomeDealerTableFSM f1) == (SomeDealerTableFSM f2) = case (f1, f2) of
    (OffDutyFSM, OffDutyFSM) -> True
    (PushingFSM t1, PushingFSM t2) -> t1 == t2
    (OnDutyFSM t1, OnDutyFSM t2) -> t1 == t2
    (LeavingFSM t1, LeavingFSM t2) -> t1 == t2
    (TaskingFSM task1, TaskingFSM task2) -> task1 == task2
    _ -> False

instance Show SomeDealerTableFSM where
  show (SomeDealerTableFSM fsm) = "SomeDealerTableFSM (" ++ show fsm ++ ")"
