{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Round where

import Data.Aeson.Types
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Pitboss.Blackjack
import Pitboss.FSM.Transitionable
import Pitboss.FSM.Types.Core

data RoundFSM
    = PeekRound SomePeekFSM
    | ENHCRound SomeENHCFSM
    deriving (Generic)

instance Transitionable RoundFSM where
    transitionType = \case
        PeekRound f -> transitionType f
        ENHCRound f -> transitionType f

mkENHCRound :: ENHCFSM p -> RoundFSM
mkENHCRound = ENHCRound . SomeENHCFSM

mkPeekRound :: PeekFSM p -> RoundFSM
mkPeekRound = PeekRound . SomePeekFSM

instance Eq RoundFSM where
    PeekRound f1 == PeekRound f2 = f1 == f2
    ENHCRound f1 == ENHCRound f2 = f1 == f2
    _ == _ = False

instance Show RoundFSM where
    show = \case
        PeekRound f -> "PeekRound (" ++ show f ++ ")"
        ENHCRound f -> "ENHCRound (" ++ show f ++ ")"

instance ToJSON RoundFSM where
    toJSON = \case
        PeekRound peekFsm ->
            object ["flavor" .= String "Peek", "state" .= toJSON peekFsm]
        ENHCRound enhcFsm ->
            object ["flavor" .= String "ENHC", "state" .= toJSON enhcFsm]

instance FromJSON RoundFSM where
    parseJSON = withObject "RoundFSM" $ \obj -> do
        flavor <- obj .: "flavor"
        case (flavor :: Text) of
            "Peek" -> PeekRound <$> obj .: "state"
            "ENHC" -> ENHCRound <$> obj .: "state"
            other -> fail $ "Unknown flavor for RoundFSM: " ++ T.unpack other

data ENHCPhase
    = ENHCAwaiting
    | ENHCBets
    | ENHCDeal
    | ENHCEarlySurrender
    | ENHCPlayers
    | ENHCDealing
    | ENHCSettle
    | ENHCComplete
    | ENHCInterrupted

data SomeENHCFSM = forall p. SomeENHCFSM (ENHCFSM p)

instance Show SomeENHCFSM where
    show (SomeENHCFSM fsm) = show fsm

instance Eq SomeENHCFSM where
    SomeENHCFSM a == SomeENHCFSM b = case (a, b) of
        (ENHCAwaitingFSM, ENHCAwaitingFSM) -> True
        (ENHCBetsFSM, ENHCBetsFSM) -> True
        (ENHCDealFSM, ENHCDealFSM) -> True
        (ENHCEarlySurrenderFSM, ENHCEarlySurrenderFSM) -> True
        (ENHCPlayersFSM, ENHCPlayersFSM) -> True
        (ENHCDealingFSM, ENHCDealingFSM) -> True
        (ENHCSettleFSM, ENHCSettleFSM) -> True
        (ENHCCompleteFSM, ENHCCompleteFSM) -> True
        _ -> False

instance ToJSON SomeENHCFSM where
    toJSON (SomeENHCFSM fsm) = case fsm of
        ENHCAwaitingFSM -> object ["tag" .= String "ENHCAwaiting"]
        ENHCBetsFSM -> object ["tag" .= String "ENHCBets"]
        ENHCDealFSM -> object ["tag" .= String "ENHCDeal"]
        ENHCEarlySurrenderFSM -> object ["tag" .= String "ENHCEarlySurrender"]
        ENHCPlayersFSM -> object ["tag" .= String "ENHCPlayers"]
        ENHCDealingFSM -> object ["tag" .= String "ENHCDealing"]
        ENHCSettleFSM -> object ["tag" .= String "ENHCSettle"]
        ENHCCompleteFSM -> object ["tag" .= String "ENHCComplete"]
        ENHCInterruptedFSM _ -> object ["tag" .= String "ENHCInterrupted"]

instance FromJSON SomeENHCFSM where
    parseJSON = withObject "SomeENHCFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "ENHCAwaiting" -> pure $ SomeENHCFSM ENHCAwaitingFSM
            "ENHCBets" -> pure $ SomeENHCFSM ENHCBetsFSM
            "ENHCDeal" -> pure $ SomeENHCFSM ENHCDealFSM
            "ENHCEarlySurrender" -> pure $ SomeENHCFSM ENHCEarlySurrenderFSM
            "ENHCPlayers" -> pure $ SomeENHCFSM ENHCPlayersFSM
            "ENHCDealing" -> pure $ SomeENHCFSM ENHCDealingFSM
            "ENHCSettle" -> pure $ SomeENHCFSM ENHCSettleFSM
            "ENHCComplete" -> pure $ SomeENHCFSM ENHCCompleteFSM
            _ -> fail $ "Unknown ENHCFSM tag: " ++ tag

instance Transitionable SomeENHCFSM where
    transitionType (SomeENHCFSM fsm) = transitionType fsm

data ENHCFSM (p :: ENHCPhase) where
    ENHCAwaitingFSM :: ENHCFSM 'ENHCAwaiting
    ENHCBetsFSM :: ENHCFSM 'ENHCBets
    ENHCDealFSM :: ENHCFSM 'ENHCDeal
    ENHCEarlySurrenderFSM :: ENHCFSM 'ENHCEarlySurrender
    ENHCPlayersFSM :: ENHCFSM 'ENHCPlayers
    ENHCDealingFSM :: ENHCFSM 'ENHCDealing
    ENHCSettleFSM :: ENHCFSM 'ENHCSettle
    ENHCCompleteFSM :: ENHCFSM 'ENHCComplete
    ENHCInterruptedFSM :: InterruptReason -> ENHCFSM 'ENHCInterrupted

deriving instance Eq (ENHCFSM p)
deriving instance Show (ENHCFSM p)

instance Transitionable (ENHCFSM p) where
    transitionType = \case
        ENHCAwaitingFSM -> AwaitInput
        ENHCBetsFSM -> AwaitInput
        ENHCDealFSM -> AutoAdvance
        ENHCEarlySurrenderFSM -> AwaitInput
        ENHCPlayersFSM -> AwaitInput
        ENHCDealingFSM -> AutoAdvance
        ENHCSettleFSM -> AutoAdvance
        ENHCCompleteFSM -> TerminalPhase
        ENHCInterruptedFSM _ -> AwaitInput

data PeekPhase
    = PeekAwaiting
    | PeekBets
    | PeekDeal
    | PeekEarlySurrender
    | PeekPeek
    | PeekInsuranceDecision
    | PeekInsuranceSettled
    | PeekContestants
    | PeekDealing
    | PeekSettle
    | PeekComplete
    | PeekInterrupted

data SomePeekFSM = forall p. SomePeekFSM (PeekFSM p)

instance Eq SomePeekFSM where
    SomePeekFSM a == SomePeekFSM b = case (a, b) of
        (PeekAwaitingFSM, PeekAwaitingFSM) -> True
        (PeekBetsFSM, PeekBetsFSM) -> True
        (PeekDealFSM, PeekDealFSM) -> True
        (PeekEarlySurrenderFSM, PeekEarlySurrenderFSM) -> True
        (PeekPeekFSM, PeekPeekFSM) -> True
        (PeekInsuranceDecisionFSM, PeekInsuranceDecisionFSM) -> True
        (PeekInsuranceSettledFSM, PeekInsuranceSettledFSM) -> True
        (PeekContestantsFSM, PeekContestantsFSM) -> True
        (PeekDealingFSM, PeekDealingFSM) -> True
        (PeekSettleFSM, PeekSettleFSM) -> True
        (PeekCompleteFSM, PeekCompleteFSM) -> True
        _ -> False

instance Show SomePeekFSM where
    show (SomePeekFSM fsm) = show fsm

instance Transitionable SomePeekFSM where
    transitionType (SomePeekFSM fsm) = transitionType fsm

instance ToJSON SomePeekFSM where
    toJSON (SomePeekFSM fsm) = case fsm of
        PeekAwaitingFSM -> object ["tag" .= String "PeekAwaiting"]
        PeekBetsFSM -> object ["tag" .= String "PeekBets"]
        PeekDealFSM -> object ["tag" .= String "PeekDeal"]
        PeekEarlySurrenderFSM -> object ["tag" .= String "PeekEarlySurrender"]
        PeekPeekFSM -> object ["tag" .= String "PeekPeek"]
        PeekInsuranceDecisionFSM -> object ["tag" .= String "PeekInsuranceDecision"]
        PeekInsuranceSettledFSM -> object ["tag" .= String "PeekInsuranceSettled"]
        PeekContestantsFSM -> object ["tag" .= String "PeekContestants"]
        PeekDealingFSM -> object ["tag" .= String "PeekDealing"]
        PeekSettleFSM -> object ["tag" .= String "PeekSettle"]
        PeekCompleteFSM -> object ["tag" .= String "PeekComplete"]
        PeekInterruptedFSM _ -> object ["tag" .= String "PeekComplete"]

instance FromJSON SomePeekFSM where
    parseJSON = withObject "SomePeekFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "PeekAwaiting" -> pure $ SomePeekFSM PeekAwaitingFSM
            "PeekBets" -> pure $ SomePeekFSM PeekBetsFSM
            "PeekDeal" -> pure $ SomePeekFSM PeekDealFSM
            "PeekEarlySurrender" -> pure $ SomePeekFSM PeekEarlySurrenderFSM
            "PeekPeek" -> pure $ SomePeekFSM PeekPeekFSM
            "PeekInsuranceDecision" -> pure $ SomePeekFSM PeekInsuranceDecisionFSM
            "PeekInsuranceSettled" -> pure $ SomePeekFSM PeekInsuranceSettledFSM
            "PeekContestants" -> pure $ SomePeekFSM PeekContestantsFSM
            "PeekDealing" -> pure $ SomePeekFSM PeekDealingFSM
            "PeekSettle" -> pure $ SomePeekFSM PeekSettleFSM
            "PeekComplete" -> pure $ SomePeekFSM PeekCompleteFSM
            _ -> fail $ "Unknown PeekFSM tag: " ++ tag

data PeekFSM (p :: PeekPhase) where
    PeekAwaitingFSM :: PeekFSM 'PeekAwaiting
    PeekBetsFSM :: PeekFSM 'PeekBets
    PeekDealFSM :: PeekFSM 'PeekDeal
    PeekEarlySurrenderFSM :: PeekFSM 'PeekEarlySurrender
    PeekPeekFSM :: PeekFSM 'PeekPeek
    PeekInsuranceDecisionFSM :: PeekFSM 'PeekInsuranceDecision
    PeekInsuranceSettledFSM :: PeekFSM 'PeekInsuranceSettled
    PeekContestantsFSM :: PeekFSM 'PeekContestants
    PeekDealingFSM :: PeekFSM 'PeekDealing
    PeekSettleFSM :: PeekFSM 'PeekSettle
    PeekCompleteFSM :: PeekFSM 'PeekComplete
    PeekInterruptedFSM :: InterruptReason -> PeekFSM 'PeekInterrupted

deriving instance Eq (PeekFSM p)
deriving instance Show (PeekFSM p)

instance Transitionable (PeekFSM p) where
    transitionType = \case
        PeekAwaitingFSM -> AwaitInput
        PeekBetsFSM -> AwaitInput
        PeekDealFSM -> AutoAdvance
        PeekEarlySurrenderFSM -> AwaitInput
        PeekPeekFSM -> AwaitInput
        PeekInsuranceDecisionFSM -> AwaitInput
        PeekInsuranceSettledFSM -> AutoAdvance
        PeekContestantsFSM -> AwaitInput
        PeekDealingFSM -> AutoAdvance
        PeekSettleFSM -> AutoAdvance
        PeekCompleteFSM -> TerminalPhase
        PeekInterruptedFSM _ -> AwaitInput
