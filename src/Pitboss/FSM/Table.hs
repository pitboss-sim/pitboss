{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.Table (
    module Pitboss.FSM.Table.FSM,
    module Pitboss.FSM.Table.Phase,
    module Pitboss.FSM.Table.Transition,
    SomeTableFSM (..),
) where

import Data.Aeson
import Data.Text qualified as T
import Pitboss.FSM.Table.FSM
import Pitboss.FSM.Table.Phase
import Pitboss.FSM.Table.Transition
import Pitboss.FSM.Types.Transitionable

data SomeTableFSM = forall p. SomeTableFSM (TableFSM p)

instance Show SomeTableFSM where
    show (SomeTableFSM fsm) = show fsm

instance Eq SomeTableFSM where
    SomeTableFSM a == SomeTableFSM b = case (a, b) of
        (ClosedFSM, ClosedFSM) -> True
        (OpeningFSM, OpeningFSM) -> True
        (RoundInProgressFSM, RoundInProgressFSM) -> True
        (IntermissionFSM, IntermissionFSM) -> True
        (ClosingFSM, ClosingFSM) -> True
        (InterruptedFSM r1, InterruptedFSM r2) -> r1 == r2
        _ -> False

instance ToJSON SomeTableFSM where
    toJSON (SomeTableFSM fsm) = case fsm of
        ClosedFSM -> object ["tag" .= String "Closed"]
        OpeningFSM -> object ["tag" .= String "Opening"]
        RoundInProgressFSM -> object ["tag" .= String "RoundInProgress"]
        IntermissionFSM -> object ["tag" .= String "Intermission"]
        ClosingFSM -> object ["tag" .= String "Closing"]
        InterruptedFSM r -> object ["tag" .= String "Interrupted", "reason" .= r]

instance FromJSON SomeTableFSM where
    parseJSON = withObject "SomeTableFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: T.Text of
            "Closed" -> pure $ SomeTableFSM ClosedFSM
            "Opening" -> pure $ SomeTableFSM OpeningFSM
            "RoundInProgress" -> pure $ SomeTableFSM RoundInProgressFSM
            "Intermission" -> pure $ SomeTableFSM IntermissionFSM
            "Closing" -> pure $ SomeTableFSM ClosingFSM
            "Interrupted" -> SomeTableFSM . InterruptedFSM <$> obj .: "reason"
            other -> fail $ "Unknown tag for SomeTableFSM: " ++ T.unpack other

instance Transitionable SomeTableFSM where
    transitionType (SomeTableFSM fsm) = transitionType fsm
