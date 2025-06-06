{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Core.Round where

import Data.Aeson.Types
import Pitboss.Blackjack

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

data PeekPhase
    = PeekAwaiting
    | PeekBets
    | PeekDeal
    | PeekEarlySurrender
    | PeekPeek
    | PeekInsuranceDecision
    | PeekInsuranceSettled
    | PeekBoutPlayers
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
        (PeekBoutPlayersFSM, PeekBoutPlayersFSM) -> True
        (PeekDealingFSM, PeekDealingFSM) -> True
        (PeekSettleFSM, PeekSettleFSM) -> True
        (PeekCompleteFSM, PeekCompleteFSM) -> True
        (PeekInterruptedFSM r1, PeekInterruptedFSM r2) -> r1 == r2
        _ -> False

instance Show SomePeekFSM where
    show (SomePeekFSM fsm) = show fsm

instance ToJSON SomePeekFSM where
    toJSON (SomePeekFSM fsm) = case fsm of
        PeekAwaitingFSM -> object ["tag" .= String "PeekAwaiting"]
        PeekBetsFSM -> object ["tag" .= String "PeekBets"]
        PeekDealFSM -> object ["tag" .= String "PeekDeal"]
        PeekEarlySurrenderFSM -> object ["tag" .= String "PeekEarlySurrender"]
        PeekPeekFSM -> object ["tag" .= String "PeekPeek"]
        PeekInsuranceDecisionFSM -> object ["tag" .= String "PeekInsuranceDecision"]
        PeekInsuranceSettledFSM -> object ["tag" .= String "PeekInsuranceSettled"]
        PeekBoutPlayersFSM -> object ["tag" .= String "PeekBoutPlayers"]
        PeekDealingFSM -> object ["tag" .= String "PeekDealing"]
        PeekSettleFSM -> object ["tag" .= String "PeekSettle"]
        PeekCompleteFSM -> object ["tag" .= String "PeekComplete"]
        PeekInterruptedFSM r -> object ["tag" .= String "PeekInterrupted", "reason" .= r]

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
            "PeekBoutPlayers" -> pure $ SomePeekFSM PeekBoutPlayersFSM
            "PeekDealing" -> pure $ SomePeekFSM PeekDealingFSM
            "PeekSettle" -> pure $ SomePeekFSM PeekSettleFSM
            "PeekComplete" -> pure $ SomePeekFSM PeekCompleteFSM
            "PeekInterrupted" -> do
                r <- obj .: "reason"
                pure $ SomePeekFSM (PeekInterruptedFSM r)
            _ -> fail $ "Unknown PeekFSM tag: " ++ tag

data PeekFSM (p :: PeekPhase) where
    PeekAwaitingFSM :: PeekFSM 'PeekAwaiting
    PeekBetsFSM :: PeekFSM 'PeekBets
    PeekDealFSM :: PeekFSM 'PeekDeal
    PeekEarlySurrenderFSM :: PeekFSM 'PeekEarlySurrender
    PeekPeekFSM :: PeekFSM 'PeekPeek
    PeekInsuranceDecisionFSM :: PeekFSM 'PeekInsuranceDecision
    PeekInsuranceSettledFSM :: PeekFSM 'PeekInsuranceSettled
    PeekBoutPlayersFSM :: PeekFSM 'PeekBoutPlayers
    PeekDealingFSM :: PeekFSM 'PeekDealing
    PeekSettleFSM :: PeekFSM 'PeekSettle
    PeekCompleteFSM :: PeekFSM 'PeekComplete
    PeekInterruptedFSM :: InterruptReason -> PeekFSM 'PeekInterrupted

deriving instance Eq (PeekFSM p)
deriving instance Show (PeekFSM p)
