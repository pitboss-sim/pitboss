{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Mechanics.Dealer.Types.DealerTableFSM where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Mechanics.Types.Transitionable
import Pitboss.Trace.Timeline.Identifier (PlayerId, TableId)

data DealerTablePhase
  = OffDuty
  | Pushing
  | OnDuty
  | Tasking DealerTask
  | Leaving
  deriving (Eq, Show, Generic)

data DealerTask
  = Cleanup
  | Shuffle
  | AttendChips
  | RespondToPlayer PlayerId
  | RespondToEnvironment
  deriving (Eq, Show, Generic)

instance ToJSON DealerTask

instance FromJSON DealerTask

data DealerTableFSM (p :: DealerTablePhase) where
  OffDutyFSM :: DealerTableFSM 'OffDuty
  PushingFSM :: TableId -> DealerTableFSM 'Pushing
  OnDutyFSM :: TableId -> DealerTableFSM 'OnDuty
  TaskingFSM :: DealerTask -> DealerTableFSM ('Tasking task)
  LeavingFSM :: TableId -> DealerTableFSM 'Leaving

deriving instance Show (DealerTableFSM p)

deriving instance Eq (DealerTableFSM p)

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

instance ToJSON SomeDealerTableFSM where
  toJSON (SomeDealerTableFSM fsm) = case fsm of
    OffDutyFSM ->
      object ["tag" .= String "OffDuty"]
    PushingFSM tid ->
      object ["tag" .= String "Pushing", "tableId" .= tid]
    OnDutyFSM tid ->
      object ["tag" .= String "OnDuty", "tableId" .= tid]
    TaskingFSM task ->
      object ["tag" .= String "Tasking", "task" .= task]
    LeavingFSM tid ->
      object ["tag" .= String "Leaving", "tableId" .= tid]

instance FromJSON SomeDealerTableFSM where
  parseJSON = withObject "SomeDealerTableFSM" $ \obj -> do
    tag <- obj .: "tag"
    case tag :: String of
      "OffDuty" -> pure $ SomeDealerTableFSM OffDutyFSM
      "Pushing" -> SomeDealerTableFSM . PushingFSM <$> obj .: "tableId"
      "OnDuty" -> SomeDealerTableFSM . OnDutyFSM <$> obj .: "tableId"
      "Tasking" -> SomeDealerTableFSM . TaskingFSM <$> obj .: "task"
      "Leaving" -> SomeDealerTableFSM . LeavingFSM <$> obj .: "tableId"
      other -> fail $ "Unknown tag for SomeDealerTableFSM: " ++ other

instance Transitionable (DealerTableFSM p) where
  transitionType = \case
    OffDutyFSM -> AwaitInput
    PushingFSM _ -> AutoAdvance
    OnDutyFSM _ -> AwaitInput
    TaskingFSM _ -> AwaitInput
    LeavingFSM _ -> AutoAdvance
