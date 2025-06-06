{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Table where

import Data.Aeson.Types
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Pitboss.FSM.Transitionable
import Pitboss.FSM.Types.Core

data TInterruptReason
    = TAttendingToPlayer
    | TPitIntervention
    | TBanking
    | TEnvironment
    deriving (Eq, Show, Generic)

instance ToJSON TInterruptReason
instance FromJSON TInterruptReason

data TablePhase
    = TClosed
    | TOpening
    | TRoundInProgress
    | TIntermission
    | TInterrupted TInterruptReason
    | TClosing
    deriving (Eq, Show, Generic)

instance ToJSON TablePhase
instance FromJSON TablePhase

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

instance Transitionable SomeTableFSM where
    transitionType (SomeTableFSM fsm) = transitionType fsm

data TableFSM (p :: TablePhase) where
    TClosedFSM :: TableFSM 'TClosed
    TOpeningFSM :: TableFSM 'TOpening
    TRoundInProgressFSM :: TableFSM 'TRoundInProgress
    TIntermissionFSM :: TableFSM 'TIntermission
    TInterruptedFSM :: TInterruptReason -> TableFSM ('TInterrupted r)
    TClosingFSM :: TableFSM 'TClosing

deriving instance Show (TableFSM p)
deriving instance Eq (TableFSM p)

instance Transitionable (TableFSM p) where
    transitionType = \case
        TClosedFSM -> AwaitInput
        TOpeningFSM -> AutoAdvance
        TRoundInProgressFSM -> AwaitInput
        TIntermissionFSM -> AutoAdvance
        TInterruptedFSM _ -> AwaitInput
        TClosingFSM -> AutoAdvance
