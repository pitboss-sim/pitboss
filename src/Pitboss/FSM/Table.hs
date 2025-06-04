{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Table where

import Data.Aeson
import Data.Text qualified as T
import GHC.Generics (Generic)
import Pitboss.FSM.Transitionable

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

type family ValidTableTransition (from :: TablePhase) (to :: TablePhase) :: Bool where
    ValidTableTransition 'TClosed 'TOpening = 'True
    ValidTableTransition 'TOpening 'TRoundInProgress = 'True
    ValidTableTransition 'TRoundInProgress 'TIntermission = 'True
    ValidTableTransition 'TIntermission 'TRoundInProgress = 'True
    ValidTableTransition 'TIntermission 'TClosing = 'True
    ValidTableTransition 'TClosing 'TClosed = 'True
    ValidTableTransition p ('TInterrupted r) = 'True
    ValidTableTransition ('TInterrupted r) 'TIntermission = 'True
    ValidTableTransition _ _ = 'False

openTable ::
    (ValidTableTransition 'TClosed 'TOpening ~ 'True) =>
    TableFSM 'TClosed ->
    TableFSM 'TOpening
openTable TClosedFSM = TOpeningFSM

beginRound ::
    (ValidTableTransition 'TOpening 'TRoundInProgress ~ 'True) =>
    TableFSM 'TOpening ->
    TableFSM 'TRoundInProgress
beginRound TOpeningFSM = TRoundInProgressFSM

pauseForTIntermission ::
    (ValidTableTransition 'TRoundInProgress 'TIntermission ~ 'True) =>
    TableFSM 'TRoundInProgress ->
    TableFSM 'TIntermission
pauseForTIntermission TRoundInProgressFSM = TIntermissionFSM

resumeRound ::
    (ValidTableTransition 'TIntermission 'TRoundInProgress ~ 'True) =>
    TableFSM 'TIntermission ->
    TableFSM 'TRoundInProgress
resumeRound TIntermissionFSM = TRoundInProgressFSM

closeTable ::
    (ValidTableTransition 'TIntermission 'TClosing ~ 'True) =>
    TableFSM 'TIntermission ->
    TableFSM 'TClosing
closeTable TIntermissionFSM = TClosingFSM

completeTClosing ::
    (ValidTableTransition 'TClosing 'TClosed ~ 'True) =>
    TableFSM 'TClosing ->
    TableFSM 'TClosed
completeTClosing TClosingFSM = TClosedFSM

interruptTable ::
    (ValidTableTransition from ('TInterrupted r) ~ 'True) =>
    TInterruptReason ->
    TableFSM from ->
    TableFSM ('TInterrupted r)
interruptTable reason _ = TInterruptedFSM reason

resumeFromTInterrupt ::
    (ValidTableTransition ('TInterrupted r) 'TIntermission ~ 'True) =>
    TableFSM ('TInterrupted r) ->
    TableFSM 'TIntermission
resumeFromTInterrupt (TInterruptedFSM _) = TIntermissionFSM
