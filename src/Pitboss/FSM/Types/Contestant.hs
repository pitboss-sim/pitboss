{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Contestant where

import Data.Aeson.Types
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Pitboss.Blackjack
import Pitboss.FSM.Transitionable
import Pitboss.FSM.Types.Core

data ContestantBoutPhase
    = CBAwaitingCards
    | CBEarlySurrender
    | CBLateSurrender
    | CBPlaying
    | CBAwaitingDealer
    | CBResolved
    | CBSettled

data SomeContestantBoutFSM = forall p. SomeContestantBoutFSM (ContestantBoutFSM p)

instance Show SomeContestantBoutFSM where
    show (SomeContestantBoutFSM fsm) = show fsm

instance Eq SomeContestantBoutFSM where
    SomeContestantBoutFSM f1 == SomeContestantBoutFSM f2 = case (f1, f2) of
        (CBAwaitingCardsFSM, CBAwaitingCardsFSM) -> True
        (CBPlayingFSM, CBPlayingFSM) -> True
        (CBAwaitingDealerFSM, CBAwaitingDealerFSM) -> True
        (CBResolvedFSM, CBResolvedFSM) -> True
        (CBSettledFSM, CBSettledFSM) -> True
        _ -> False

instance ToJSON SomeContestantBoutFSM where
    toJSON (SomeContestantBoutFSM fsm) = case fsm of
        CBAwaitingCardsFSM -> object ["tag" .= String "AwaitingCards"]
        CBPlayingFSM -> object ["tag" .= String "Playing"]
        CBAwaitingDealerFSM -> object ["tag" .= String "AwaitingDealer"]
        CBResolvedFSM -> object ["tag" .= String "Resolved"]
        CBSettledFSM -> object ["tag" .= String "Settled"]

instance FromJSON SomeContestantBoutFSM where
    parseJSON = withObject "SomeContestantBoutFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "AwaitingCards" -> pure $ SomeContestantBoutFSM CBAwaitingCardsFSM
            "Playing" -> pure $ SomeContestantBoutFSM CBPlayingFSM
            "AwaitingDealer" -> pure $ SomeContestantBoutFSM CBAwaitingDealerFSM
            "Resolved" -> pure $ SomeContestantBoutFSM CBResolvedFSM
            "Settled" -> pure $ SomeContestantBoutFSM CBSettledFSM
            _ -> fail $ "Unknown ContestantBoutFSM tag: " ++ tag

instance Transitionable SomeContestantBoutFSM where
    transitionType (SomeContestantBoutFSM fsm) = transitionType fsm

data ContestantBoutFSM (p :: ContestantBoutPhase) where
    CBAwaitingCardsFSM :: ContestantBoutFSM 'CBAwaitingCards
    CBPlayingFSM :: ContestantBoutFSM 'CBPlaying
    CBAwaitingDealerFSM :: ContestantBoutFSM 'CBAwaitingDealer
    CBResolvedFSM :: ContestantBoutFSM 'CBResolved
    CBSettledFSM :: ContestantBoutFSM 'CBSettled

deriving instance Show (ContestantBoutFSM p)
deriving instance Eq (ContestantBoutFSM p)

instance Transitionable (ContestantBoutFSM p) where
    transitionType = \case
        CBAwaitingCardsFSM -> AwaitInput
        CBPlayingFSM -> AwaitInput
        CBAwaitingDealerFSM -> AwaitInput
        CBResolvedFSM -> AutoAdvance
        CBSettledFSM -> TerminalPhase

data HandPhase
    = CHAwaitingFirstCard
    | CHAwaitingSecondCard
    | CHDecision
    | CHHitting
    | CHAwaitingOneCard OneCardDrawReason
    | CHResolved ContestantHandResolution
    | CHAbandoned AbandonedReason
    deriving (Eq, Show, Generic)

instance ToJSON HandPhase where
    toJSON = \case
        CHAwaitingFirstCard -> object ["tag" .= String "AwaitingFirstCard"]
        CHAwaitingSecondCard -> object ["tag" .= String "AwaitingSecondCard"]
        CHDecision -> object ["tag" .= String "Decision"]
        CHHitting -> object ["tag" .= String "Hitting"]
        CHAwaitingOneCard reason -> object ["tag" .= String "AwaitingOneCard", "reason" .= reason]
        CHResolved res -> object ["tag" .= String "Resolved", "resolution" .= res]
        CHAbandoned r -> object ["tag" .= String "Abandoned", "reason" .= r]

instance FromJSON HandPhase where
    parseJSON = withObject "HandPhase" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: Text of
            "AwaitingFirstCard" -> pure CHAwaitingFirstCard
            "AwaitingSecondCard" -> pure CHAwaitingSecondCard
            "Decision" -> pure CHDecision
            "Hitting" -> pure CHHitting
            "AwaitingOneCard" -> CHAwaitingOneCard <$> obj .: "reason"
            "Resolved" -> CHResolved <$> obj .: "resolution"
            "Abandoned" -> CHAbandoned <$> obj .: "reason"
            _ -> fail $ "Unknown HandPhase tag: " ++ T.unpack tag

data SomeContestantHandFSM = forall p h d s. SomeContestantHandFSM (ContestantHandFSM p h d s)

instance Show SomeContestantHandFSM where
    show (SomeContestantHandFSM fsm) = "SomeContestantHandFSM (" ++ show fsm ++ ")"

instance Eq SomeContestantHandFSM where
    (SomeContestantHandFSM f1) == (SomeContestantHandFSM f2) = case (f1, f2) of
        (CHAwaitingFirstCardFSM, CHAwaitingFirstCardFSM) -> True
        (CHAwaitingSecondCardFSM, CHAwaitingSecondCardFSM) -> True
        (CHDecisionFSM, CHDecisionFSM) -> True
        (CHHittingFSM, CHHittingFSM) -> True
        (CHAwaitingOneCardFSM r1, CHAwaitingOneCardFSM r2) -> r1 == r2
        (CHResolvedFSM r1, CHResolvedFSM r2) -> r1 == r2
        (CHAbandonedFSM r1, CHAbandonedFSM r2) -> r1 == r2
        _ -> False

instance ToJSON SomeContestantHandFSM where
    toJSON (SomeContestantHandFSM fsm) = case fsm of
        CHAwaitingFirstCardFSM ->
            object ["tag" .= String "AwaitingFirstCard"]
        CHAwaitingSecondCardFSM ->
            object ["tag" .= String "AwaitingSecondCard"]
        CHDecisionFSM ->
            object ["tag" .= String "Decision"]
        CHHittingFSM ->
            object ["tag" .= String "Hitting"]
        CHAwaitingOneCardFSM reason ->
            object ["tag" .= String "AwaitingOneCard", "reason" .= reason]
        CHResolvedFSM resolution ->
            object ["tag" .= String "Resolved", "resolution" .= resolution]
        CHAbandonedFSM reason ->
            object ["tag" .= String "Abandoned", "reason" .= reason]

instance FromJSON SomeContestantHandFSM where
    parseJSON = withObject "SomeContestantHandFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: Text of
            "AwaitingFirstCard" -> pure $ SomeContestantHandFSM CHAwaitingFirstCardFSM
            "AwaitingSecondCard" -> pure $ SomeContestantHandFSM CHAwaitingSecondCardFSM
            "Decision" -> pure $ SomeContestantHandFSM CHDecisionFSM
            "Hitting" -> pure $ SomeContestantHandFSM CHHittingFSM
            "AwaitingOneCard" -> SomeContestantHandFSM . CHAwaitingOneCardFSM <$> obj .: "reason"
            "Resolved" -> SomeContestantHandFSM . CHResolvedFSM <$> obj .: "resolution"
            "Abandoned" -> SomeContestantHandFSM . CHAbandonedFSM <$> obj .: "reason"
            _ -> fail $ "Unknown tag for SomeContestantHandFSM: " ++ T.unpack tag

instance Transitionable SomeContestantHandFSM where
    transitionType (SomeContestantHandFSM fsm) = transitionType fsm

data OHit = OKHit | NoHit
data ODbl = OKDbl | NoDbl
data OSpl = OKSpl | NoSpl

data ContestantHandFSM (p :: HandPhase) (h :: OHit) (d :: ODbl) (s :: OSpl) where
    CHAwaitingFirstCardFSM :: ContestantHandFSM 'CHAwaitingFirstCard 'NoHit 'NoDbl 'NoSpl
    CHAwaitingSecondCardFSM :: ContestantHandFSM 'CHAwaitingSecondCard 'NoHit 'NoDbl 'NoSpl
    CHDecisionFSM :: ContestantHandFSM 'CHDecision h d s
    CHHittingFSM :: ContestantHandFSM 'CHHitting h d s
    CHAwaitingOneCardFSM :: OneCardDrawReason -> ContestantHandFSM ('CHAwaitingOneCard reason) 'NoHit 'NoDbl 'NoSpl
    CHResolvedFSM :: ContestantHandResolution -> ContestantHandFSM ('CHResolved res) 'NoHit 'NoDbl 'NoSpl
    CHAbandonedFSM :: AbandonedReason -> ContestantHandFSM ('CHAbandoned reason) 'NoHit 'NoDbl 'NoSpl

deriving instance Show (ContestantHandFSM p h d s)
deriving instance Eq (ContestantHandFSM p h d s)

instance Transitionable (ContestantHandFSM p h d s) where
    transitionType = \case
        CHAwaitingFirstCardFSM -> AwaitInput
        CHAwaitingSecondCardFSM -> AwaitInput
        CHDecisionFSM -> AwaitInput
        CHHittingFSM -> AwaitInput
        CHAwaitingOneCardFSM _ -> AutoAdvance
        CHResolvedFSM _ -> TerminalPhase
        CHAbandonedFSM _ -> TerminalPhase

data ContestantRoundPhase
    = CRIdle
    | CREngaged
    | CRWaitingForHands
    | CRResolved
    | CRInterrupted InterruptReason
    deriving (Eq, Show, Generic)

instance ToJSON ContestantRoundPhase where
    toJSON = \case
        CRIdle -> object ["tag" .= String "Idle"]
        CREngaged -> object ["tag" .= String "Engaged"]
        CRWaitingForHands -> object ["tag" .= String "WaitingForHands"]
        CRResolved -> object ["tag" .= String "Resolved"]
        CRInterrupted reason -> object ["tag" .= String "Interrupted", "reason" .= reason]

instance FromJSON ContestantRoundPhase where
    parseJSON = withObject "ContestantRoundPhase" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: Text of
            "Idle" -> pure CRIdle
            "Engaged" -> pure CREngaged
            "WaitingForHands" -> pure CRWaitingForHands
            "Resolved" -> pure CRResolved
            "Interrupted" -> CRInterrupted <$> obj .: "reason"
            _ -> fail $ "Unknown tag in ContestantRoundPhase object: " ++ T.unpack tag

data SomeContestantRoundFSM = forall p. SomeContestantRoundFSM (ContestantRoundFSM p)

instance Show SomeContestantRoundFSM where
    show (SomeContestantRoundFSM fsm) = show fsm

instance Eq SomeContestantRoundFSM where
    SomeContestantRoundFSM f1 == SomeContestantRoundFSM f2 = case (f1, f2) of
        (CRIdleFSM, CRIdleFSM) -> True
        (CREngagedFSM, CREngagedFSM) -> True
        (CRWaitingForHandsFSM, CRWaitingForHandsFSM) -> True
        (CRResolvedFSM, CRResolvedFSM) -> True
        (CRInterruptedFSM r1, CRInterruptedFSM r2) -> r1 == r2
        _ -> False

instance ToJSON SomeContestantRoundFSM where
    toJSON (SomeContestantRoundFSM fsm) = case fsm of
        CRIdleFSM -> object ["tag" .= String "Idle"]
        CREngagedFSM -> object ["tag" .= String "Engaged"]
        CRWaitingForHandsFSM -> object ["tag" .= String "WaitingForHands"]
        CRResolvedFSM -> object ["tag" .= String "Resolved"]
        CRInterruptedFSM reason ->
            object
                [ "tag" .= String "Interrupted"
                , "reason" .= reason
                ]

instance FromJSON SomeContestantRoundFSM where
    parseJSON = withObject "SomeContestantRoundFSM" $ \obj -> do
        tag <- obj .: "tag"
        case (tag :: Text) of
            "Idle" -> pure $ SomeContestantRoundFSM CRIdleFSM
            "Engaged" -> pure $ SomeContestantRoundFSM CREngagedFSM
            "WaitingForHands" -> pure $ SomeContestantRoundFSM CRWaitingForHandsFSM
            "Resolved" -> pure $ SomeContestantRoundFSM CRResolvedFSM
            "Interrupted" -> do
                reason <- obj .: "reason"
                pure $ SomeContestantRoundFSM (CRInterruptedFSM reason)
            other -> fail $ "Unknown tag for SomeContestantRoundFSM: " ++ T.unpack other

instance Transitionable SomeContestantRoundFSM where
    transitionType (SomeContestantRoundFSM fsm) = transitionType fsm

data ContestantRoundFSM (p :: ContestantRoundPhase) where
    CRIdleFSM :: ContestantRoundFSM 'CRIdle
    CREngagedFSM :: ContestantRoundFSM 'CREngaged
    CRWaitingForHandsFSM :: ContestantRoundFSM 'CRWaitingForHands
    CRResolvedFSM :: ContestantRoundFSM 'CRResolved
    CRInterruptedFSM :: InterruptReason -> ContestantRoundFSM ('CRInterrupted r)

deriving instance Show (ContestantRoundFSM p)
deriving instance Eq (ContestantRoundFSM p)

instance Transitionable (ContestantRoundFSM p) where
    transitionType = \case
        CRIdleFSM -> AwaitInput
        CREngagedFSM -> AwaitInput
        CRWaitingForHandsFSM -> AwaitInput
        CRResolvedFSM -> TerminalPhase
        CRInterruptedFSM _ -> AwaitInput
