{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.DealerTable where

import Data.Aeson.Types
import GHC.Generics (Generic)

data DealerTask
    = DTCleanup
    | DTShuffle
    | DTAttendChips
    | DTRespondToPlayer
    | DTRespondToEnvironment
    deriving (Eq, Show, Generic)

data DealerTablePhase
    = DTOffDuty
    | DTPushing
    | DTOnDuty
    | DTTasking DealerTask
    | DTLeaving
    deriving (Eq, Show, Generic)

instance ToJSON DealerTask
instance FromJSON DealerTask
instance ToJSON DealerTablePhase
instance FromJSON DealerTablePhase

data SomeDealerTableFSM = forall p. SomeDealerTableFSM (DealerTableFSM p)

data DealerTableFSM (p :: DealerTablePhase) where
    DTOffDutyFSM :: DealerTableFSM 'DTOffDuty
    DTPushingFSM :: DealerTableFSM 'DTPushing
    DTOnDutyFSM :: DealerTableFSM 'DTOnDuty
    DTTaskingFSM :: DealerTask -> DealerTableFSM ('DTTasking task)
    DTLeavingFSM :: DealerTableFSM 'DTLeaving

deriving instance Eq (DealerTableFSM p)
deriving instance Show (DealerTableFSM p)

instance Eq SomeDealerTableFSM where
    (SomeDealerTableFSM f1) == (SomeDealerTableFSM f2) = case (f1, f2) of
        (DTOffDutyFSM, DTOffDutyFSM) -> True
        (DTPushingFSM, DTPushingFSM) -> True
        (DTOnDutyFSM, DTOnDutyFSM) -> True
        (DTLeavingFSM, DTLeavingFSM) -> True
        (DTTaskingFSM task1, DTTaskingFSM task2) -> task1 == task2
        _ -> False

instance Show SomeDealerTableFSM where
    show (SomeDealerTableFSM fsm) = "SomeDealerTableFSM (" ++ show fsm ++ ")"

instance ToJSON SomeDealerTableFSM where
    toJSON (SomeDealerTableFSM fsm) = case fsm of
        DTOffDutyFSM -> object ["tag" .= String "OffDuty"]
        DTPushingFSM -> object ["tag" .= String "Pushing"]
        DTOnDutyFSM -> object ["tag" .= String "OnDuty"]
        DTTaskingFSM task -> object ["tag" .= String "Tasking", "task" .= task]
        DTLeavingFSM -> object ["tag" .= String "Leaving"]

instance FromJSON SomeDealerTableFSM where
    parseJSON = withObject "SomeDealerTableFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: String of
            "OffDuty" -> pure $ SomeDealerTableFSM DTOffDutyFSM
            "Pushing" -> pure $ SomeDealerTableFSM DTPushingFSM
            "OnDuty" -> pure $ SomeDealerTableFSM DTOnDutyFSM
            "Tasking" -> SomeDealerTableFSM . DTTaskingFSM <$> obj .: "task"
            "Leaving" -> pure $ SomeDealerTableFSM DTLeavingFSM
            other -> fail $ "Unknown tag for SomeDealerTableFSM: " ++ other

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
