{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.DealerTable
  ( module Pitboss.FSM.DealerTable.FSM,
    module Pitboss.FSM.DealerTable.Phase,
    module Pitboss.FSM.DealerTable.Transition,
    SomeDealerTableFSM (..),
    mkDealerTableFSMOffDuty,
    mkDealerTableFSMOnDuty,
    mkDealerTableFSMPushing,
    mkDealerTableFSMTasking,
    mkDealerTableFSMLeaving,
  )
where

import Data.Aeson.Types
import Pitboss.FSM.DealerTable.FSM
import Pitboss.FSM.DealerTable.Phase
import Pitboss.FSM.DealerTable.Transition

mkDealerTableFSMOffDuty :: SomeDealerTableFSM
mkDealerTableFSMOffDuty = SomeDealerTableFSM OffDutyFSM

mkDealerTableFSMOnDuty :: SomeDealerTableFSM
mkDealerTableFSMOnDuty = SomeDealerTableFSM OnDutyFSM

mkDealerTableFSMPushing :: SomeDealerTableFSM
mkDealerTableFSMPushing = SomeDealerTableFSM PushingFSM

mkDealerTableFSMTasking :: DealerTask -> SomeDealerTableFSM
mkDealerTableFSMTasking task = SomeDealerTableFSM (TaskingFSM task)

mkDealerTableFSMLeaving :: SomeDealerTableFSM
mkDealerTableFSMLeaving = SomeDealerTableFSM LeavingFSM

data SomeDealerTableFSM = forall p. SomeDealerTableFSM (DealerTableFSM p)

instance Eq SomeDealerTableFSM where
  (SomeDealerTableFSM f1) == (SomeDealerTableFSM f2) = case (f1, f2) of
    (OffDutyFSM, OffDutyFSM) -> True
    (PushingFSM, PushingFSM) -> True
    (OnDutyFSM, OnDutyFSM) -> True
    (LeavingFSM, LeavingFSM) -> True
    (TaskingFSM task1, TaskingFSM task2) -> task1 == task2
    _ -> False

instance Show SomeDealerTableFSM where
  show (SomeDealerTableFSM fsm) = "SomeDealerTableFSM (" ++ show fsm ++ ")"

instance ToJSON SomeDealerTableFSM where
  toJSON (SomeDealerTableFSM fsm) = case fsm of
    OffDutyFSM ->
      object ["tag" .= String "OffDuty"]
    PushingFSM ->
      object ["tag" .= String "Pushing"]
    OnDutyFSM ->
      object ["tag" .= String "OnDuty"]
    TaskingFSM task ->
      object ["tag" .= String "Tasking", "task" .= task]
    LeavingFSM ->
      object ["tag" .= String "Leaving"]

instance FromJSON SomeDealerTableFSM where
  parseJSON = withObject "SomeDealerTableFSM" $ \obj -> do
    tag <- obj .: "tag"
    case tag :: String of
      "OffDuty" -> pure $ SomeDealerTableFSM OffDutyFSM
      "Pushing" -> pure $ SomeDealerTableFSM PushingFSM
      "OnDuty" -> pure $ SomeDealerTableFSM OnDutyFSM
      "Tasking" -> SomeDealerTableFSM . TaskingFSM <$> obj .: "task"
      "Leaving" -> pure $ SomeDealerTableFSM LeavingFSM
      other -> fail $ "Unknown tag for SomeDealerTableFSM: " ++ other
