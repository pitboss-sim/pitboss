{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Dealer where

import Data.Aeson.Types
import Pitboss.FSM.Types.Core

data SomeDealerTableFSM = forall p. SomeDealerTableFSM (DealerTableFSM p)

instance Eq SomeDealerTableFSM where
    (SomeDealerTableFSM f1) == (SomeDealerTableFSM f2) = case (f1, f2) of
        (DOffDutyFSM, DOffDutyFSM) -> True
        (DPushingFSM, DPushingFSM) -> True
        (DOnDutyFSM, DOnDutyFSM) -> True
        (DLeavingFSM, DLeavingFSM) -> True
        (DTaskingFSM task1, DTaskingFSM task2) -> task1 == task2
        _ -> False

instance Show SomeDealerTableFSM where
    show (SomeDealerTableFSM fsm) = "SomeDealerTableFSM (" ++ show fsm ++ ")"

instance ToJSON SomeDealerTableFSM where
    toJSON (SomeDealerTableFSM fsm) = case fsm of
        DOffDutyFSM -> object ["tag" .= String "OffDuty"]
        DPushingFSM -> object ["tag" .= String "Pushing"]
        DOnDutyFSM -> object ["tag" .= String "OnDuty"]
        DTaskingFSM task -> object ["tag" .= String "Tasking", "task" .= task]
        DLeavingFSM -> object ["tag" .= String "Leaving"]

instance FromJSON SomeDealerTableFSM where
    parseJSON = withObject "SomeDealerTableFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: String of
            "OffDuty" -> pure $ SomeDealerTableFSM DOffDutyFSM
            "Pushing" -> pure $ SomeDealerTableFSM DPushingFSM
            "OnDuty" -> pure $ SomeDealerTableFSM DOnDutyFSM
            "Tasking" -> SomeDealerTableFSM . DTaskingFSM <$> obj .: "task"
            "Leaving" -> pure $ SomeDealerTableFSM DLeavingFSM
            other -> fail $ "Unknown tag for SomeDealerTableFSM: " ++ other
