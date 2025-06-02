{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.DealerTable (
    module Pitboss.FSM.DealerTable.FSM,
    module Pitboss.FSM.DealerTable.Phase,
    module Pitboss.FSM.DealerTable.Transition,
    SomeDealerTableFSM (..),
)
where

import Data.Aeson.Types
import Pitboss.FSM.DealerTable.FSM
import Pitboss.FSM.DealerTable.Phase
import Pitboss.FSM.DealerTable.Transition

data SomeDealerTableFSM = forall p. SomeDealerTableFSM (DealerTableFSM p)

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
