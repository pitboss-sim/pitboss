{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Dealer where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack
import Pitboss.FSM.Transitionable
import Pitboss.FSM.Types.Core (TransitionPhase (..))

data DealerTask
    = DTCleanup
    | DTShuffle
    | DTAttendChips
    | DTRespondToPlayer
    | DTRespondToEnvironment
    deriving (Eq, Show, Generic)

instance ToJSON DealerTask
instance FromJSON DealerTask

data DealerTablePhase
    = DTOffDuty
    | DTPushing
    | DTOnDuty
    | DTTasking DealerTask
    | DTLeaving
    deriving (Eq, Show, Generic)

instance ToJSON DealerTablePhase
instance FromJSON DealerTablePhase

data SomeDealerTableFSM = forall p. SomeDealerTableFSM (DealerTableFSM p)

instance Transitionable SomeDealerTableFSM where
    transitionType (SomeDealerTableFSM fsm) = transitionType fsm

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

data DealerTableFSM (p :: DealerTablePhase) where
    DTOffDutyFSM :: DealerTableFSM 'DTOffDuty
    DTPushingFSM :: DealerTableFSM 'DTPushing
    DTOnDutyFSM :: DealerTableFSM 'DTOnDuty
    DTTaskingFSM :: DealerTask -> DealerTableFSM ('DTTasking task)
    DTLeavingFSM :: DealerTableFSM 'DTLeaving

deriving instance Eq (DealerTableFSM p)
deriving instance Show (DealerTableFSM p)

instance Transitionable (DealerTableFSM p) where
    transitionType = \case
        DTOffDutyFSM -> AwaitInput
        DTPushingFSM -> AutoAdvance
        DTOnDutyFSM -> AwaitInput
        DTTaskingFSM _ -> AwaitInput
        DTLeavingFSM -> AutoAdvance

data DealerHandPhase
    = DHDealing
    | DHEvaluating
    | DHResolved DealerHandResolution
    | DHInterrupted InterruptReason
    deriving (Eq, Show, Generic)

instance ToJSON DealerHandPhase
instance FromJSON DealerHandPhase

data SomeDealerHandFSM = forall p. SomeDealerHandFSM (DealerHandFSM p)

instance Show SomeDealerHandFSM where
    show (SomeDealerHandFSM fsm) = "SomeDealerHandFSM (" ++ show fsm ++ ")"

instance Eq SomeDealerHandFSM where
    (SomeDealerHandFSM f1) == (SomeDealerHandFSM f2) = case (f1, f2) of
        (DHDealingFSM, DHDealingFSM) -> True
        (DHEvaluatingFSM, DHEvaluatingFSM) -> True
        (DHResolvedFSM r1, DHResolvedFSM r2) -> r1 == r2
        (DHInterruptedFSM r1, DHInterruptedFSM r2) -> r1 == r2
        _ -> False

instance ToJSON SomeDealerHandFSM where
    toJSON (SomeDealerHandFSM fsm) = case fsm of
        DHDealingFSM -> object ["tag" .= String "Dealing"]
        DHEvaluatingFSM -> object ["tag" .= String "Evaluating"]
        DHResolvedFSM r -> object ["tag" .= String "Resolved", "resolution" .= r]
        DHInterruptedFSM r -> object ["tag" .= String "Interrupted", "reason" .= r]

instance FromJSON SomeDealerHandFSM where
    parseJSON = withObject "SomeDealerHandFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "Dealing" -> pure $ SomeDealerHandFSM DHDealingFSM
            "Evaluating" -> pure $ SomeDealerHandFSM DHEvaluatingFSM
            "Resolved" -> do
                r <- obj .: "resolution"
                pure $ SomeDealerHandFSM (DHResolvedFSM r)
            "Interrupted" -> do
                r <- obj .: "reason"
                pure $ SomeDealerHandFSM (DHInterruptedFSM r)
            other -> fail $ "Unknown tag for SomeDealerHandFSM: " ++ other

instance Transitionable SomeDealerHandFSM where
    transitionType (SomeDealerHandFSM fsm) = transitionType fsm

data DealerHandFSM (p :: DealerHandPhase) where
    DHDealingFSM :: DealerHandFSM 'DHDealing
    DHEvaluatingFSM :: DealerHandFSM 'DHEvaluating
    DHResolvedFSM :: DealerHandResolution -> DealerHandFSM ('DHResolved r)
    DHInterruptedFSM :: InterruptReason -> DealerHandFSM ('DHInterrupted r)

deriving instance Show (DealerHandFSM p)
deriving instance Eq (DealerHandFSM p)

instance Transitionable (DealerHandFSM p) where
    transitionType = \case
        DHDealingFSM -> AwaitInput
        DHEvaluatingFSM -> AutoAdvance
        DHResolvedFSM _ -> TerminalPhase
        DHInterruptedFSM _ -> AwaitInput
