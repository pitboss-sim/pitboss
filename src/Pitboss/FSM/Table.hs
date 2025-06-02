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
        (TClosedFSM, TClosedFSM) -> True
        (TOpeningFSM, TOpeningFSM) -> True
        (TRoundInProgressFSM, TRoundInProgressFSM) -> True
        (TIntermissionFSM, TIntermissionFSM) -> True
        (TClosingFSM, TClosingFSM) -> True
        (TInterruptedFSM r1, TInterruptedFSM r2) -> r1 == r2
        _ -> False

instance ToJSON SomeTableFSM where
    toJSON (SomeTableFSM fsm) = case fsm of
        TClosedFSM -> object ["tag" .= String "Closed"]
        TOpeningFSM -> object ["tag" .= String "Opening"]
        TRoundInProgressFSM -> object ["tag" .= String "RoundInProgress"]
        TIntermissionFSM -> object ["tag" .= String "Intermission"]
        TClosingFSM -> object ["tag" .= String "Closing"]
        TInterruptedFSM r -> object ["tag" .= String "Interrupted", "reason" .= r]

instance FromJSON SomeTableFSM where
    parseJSON = withObject "SomeTableFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: T.Text of
            "Closed" -> pure $ SomeTableFSM TClosedFSM
            "Opening" -> pure $ SomeTableFSM TOpeningFSM
            "RoundInProgress" -> pure $ SomeTableFSM TRoundInProgressFSM
            "Intermission" -> pure $ SomeTableFSM TIntermissionFSM
            "Closing" -> pure $ SomeTableFSM TClosingFSM
            "Interrupted" -> SomeTableFSM . TInterruptedFSM <$> obj .: "reason"
            other -> fail $ "Unknown tag for SomeTableFSM: " ++ T.unpack other

instance Transitionable SomeTableFSM where
    transitionType (SomeTableFSM fsm) = transitionType fsm
