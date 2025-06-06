{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Table where

import Data.Aeson.Types
import Data.Text (Text)
import Data.Text qualified as T
import Pitboss.FSM.Types.Core

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
        case tag :: Text of
            "Closed" -> pure $ SomeTableFSM TClosedFSM
            "Opening" -> pure $ SomeTableFSM TOpeningFSM
            "RoundInProgress" -> pure $ SomeTableFSM TRoundInProgressFSM
            "Intermission" -> pure $ SomeTableFSM TIntermissionFSM
            "Closing" -> pure $ SomeTableFSM TClosingFSM
            "Interrupted" -> SomeTableFSM . TInterruptedFSM <$> obj .: "reason"
            other -> fail $ "Unknown tag for SomeTableFSM: " ++ T.unpack other
