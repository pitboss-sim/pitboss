{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pitboss.FSM.DealerTableFSM.JSON where

import Data.Aeson
import Pitboss.FSM.DealerTableFSM.Existential
import Pitboss.FSM.DealerTableFSM.Types

instance ToJSON DealerTask

instance FromJSON DealerTask

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
