{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Mechanics.Dealer.Types.DealerRoundFSM where

import Data.Aeson
import Data.Text qualified as T
import Pitboss.Mechanics.Types.PhaseTag
import Pitboss.Mechanics.Types.Transitionable

data DealerRoundFlavor = IsPeek | IsENHC

data DealerRoundPhase
  = Awaiting
  | Bets
  | Deal
  | EarlySurrender
  | InsuranceDecision
  | InsuranceSettled
  | Peek
  | Players
  | Dealer
  | Settle
  | Complete

data DealerRoundFSM
  = PeekDealerRound SomePeekFSM
  | ENHCDealerRound SomeENHCFSM

instance Eq DealerRoundFSM where
  PeekDealerRound f1 == PeekDealerRound f2 = f1 == f2
  ENHCDealerRound f1 == ENHCDealerRound f2 = f1 == f2
  _ == _ = False

instance Show DealerRoundFSM where
  show = \case
    PeekDealerRound fsm -> "PeekDealerRound (" ++ show fsm ++ ")"
    ENHCDealerRound fsm -> "ENHCDealerRound (" ++ show fsm ++ ")"

class AtDecisionPoint fsm where
  toPlayersPhase :: fsm -> Maybe fsm

instance AtDecisionPoint (PeekFSM p) where
  toPlayersPhase = \case
    PeekPlayersFSM -> Just PeekPlayersFSM
    _ -> Nothing

instance AtDecisionPoint (ENHCFSM p) where
  toPlayersPhase = \case
    ENHCPlayersFSM -> Just ENHCPlayersFSM
    _ -> Nothing

instance Transitionable DealerRoundFSM where
  transitionType = \case
    PeekDealerRound f -> transitionType f
    ENHCDealerRound f -> transitionType f

newtype SomeDealerRoundFSM = SomeDealerRoundFSM DealerRoundFSM

instance Eq SomeDealerRoundFSM where
  (SomeDealerRoundFSM f1) == (SomeDealerRoundFSM f2) = f1 == f2

instance Show SomeDealerRoundFSM where
  show (SomeDealerRoundFSM fsm) = "SomeDealerRoundFSM (" ++ show fsm ++ ")"

instance ToJSON SomeDealerRoundFSM where
  toJSON (SomeDealerRoundFSM fsm) = case fsm of
    PeekDealerRound peekFsm ->
      object ["flavor" .= String "Peek", "state" .= toJSON peekFsm]
    ENHCDealerRound enhcFsm ->
      object ["flavor" .= String "ENHC", "state" .= toJSON enhcFsm]

instance FromJSON SomeDealerRoundFSM where
  parseJSON = withObject "SomeDealerRoundFSM" $ \obj -> do
    flavor <- obj .: "flavor"
    case (flavor :: T.Text) of
      "Peek" -> do
        peek <- obj .: "state"
        SomeDealerRoundFSM . PeekDealerRound <$> parseJSON peek
      "ENHC" -> do
        enhc <- obj .: "state"
        SomeDealerRoundFSM . ENHCDealerRound <$> parseJSON enhc
      other -> fail $ "Unknown flavor for DealerRoundFSM: " ++ T.unpack other

data ENHCPhase
  = ENHCAwaiting
  | ENHCBets
  | ENHCDeal
  | ENHCEarlySurrender
  | ENHCPlayers
  | ENHCDealer
  | ENHCSettle
  | ENHCComplete

data ENHCFSM (p :: ENHCPhase) where
  ENHCAwaitingFSM :: ENHCFSM 'ENHCAwaiting
  ENHCBetsFSM :: ENHCFSM 'ENHCBets
  ENHCDealFSM :: ENHCFSM 'ENHCDeal
  ENHCEarlySurrenderFSM :: ENHCFSM 'ENHCEarlySurrender
  ENHCPlayersFSM :: ENHCFSM 'ENHCPlayers
  ENHCDealerFSM :: ENHCFSM 'ENHCDealer
  ENHCSettleFSM :: ENHCFSM 'ENHCSettle
  ENHCCompleteFSM :: ENHCFSM 'ENHCComplete

deriving instance Show (ENHCFSM p)

deriving instance Eq (ENHCFSM p)

instance PhaseTag ENHCFSM DealerRoundPhase where
  phaseTag = \case
    ENHCAwaitingFSM -> Awaiting
    ENHCBetsFSM -> Bets
    ENHCDealFSM -> Deal
    ENHCEarlySurrenderFSM -> EarlySurrender
    ENHCPlayersFSM -> Players
    ENHCDealerFSM -> Dealer
    ENHCSettleFSM -> Settle
    ENHCCompleteFSM -> Complete

instance Transitionable (ENHCFSM p) where
  transitionType = \case
    ENHCAwaitingFSM -> AwaitInput
    ENHCBetsFSM -> AwaitInput
    ENHCDealFSM -> AutoAdvance
    ENHCEarlySurrenderFSM -> AwaitInput
    ENHCPlayersFSM -> AwaitInput
    ENHCDealerFSM -> AutoAdvance
    ENHCSettleFSM -> AutoAdvance
    ENHCCompleteFSM -> TerminalPhase

data SomeENHCFSM = forall p. SomeENHCFSM (ENHCFSM p)

instance Transitionable SomeENHCFSM where
  transitionType (SomeENHCFSM fsm) = transitionType fsm

instance Show SomeENHCFSM where
  show (SomeENHCFSM fsm) = show fsm

instance Eq SomeENHCFSM where
  SomeENHCFSM a == SomeENHCFSM b = case (a, b) of
    (ENHCAwaitingFSM, ENHCAwaitingFSM) -> True
    (ENHCBetsFSM, ENHCBetsFSM) -> True
    (ENHCDealFSM, ENHCDealFSM) -> True
    (ENHCEarlySurrenderFSM, ENHCEarlySurrenderFSM) -> True
    (ENHCPlayersFSM, ENHCPlayersFSM) -> True
    (ENHCDealerFSM, ENHCDealerFSM) -> True
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
    ENHCDealerFSM -> object ["tag" .= String "ENHCDealer"]
    ENHCSettleFSM -> object ["tag" .= String "ENHCSettle"]
    ENHCCompleteFSM -> object ["tag" .= String "ENHCComplete"]

instance FromJSON SomeENHCFSM where
  parseJSON = withObject "SomeENHCFSM" $ \obj -> do
    tag <- obj .: "tag"
    case tag of
      "ENHCAwaiting" -> pure $ SomeENHCFSM ENHCAwaitingFSM
      "ENHCBets" -> pure $ SomeENHCFSM ENHCBetsFSM
      "ENHCDeal" -> pure $ SomeENHCFSM ENHCDealFSM
      "ENHCEarlySurrender" -> pure $ SomeENHCFSM ENHCEarlySurrenderFSM
      "ENHCPlayers" -> pure $ SomeENHCFSM ENHCPlayersFSM
      "ENHCDealer" -> pure $ SomeENHCFSM ENHCDealerFSM
      "ENHCSettle" -> pure $ SomeENHCFSM ENHCSettleFSM
      "ENHCComplete" -> pure $ SomeENHCFSM ENHCCompleteFSM
      _ -> fail $ "Unknown ENHCFSM tag: " ++ tag

data PeekFSM (p :: PeekPhase) where
  PeekAwaitingFSM :: PeekFSM 'PeekAwaiting
  PeekBetsFSM :: PeekFSM 'PeekBets
  PeekDealFSM :: PeekFSM 'PeekDeal
  PeekEarlySurrenderFSM :: PeekFSM 'PeekEarlySurrender
  PeekPeekFSM :: PeekFSM 'PeekPeek
  PeekInsuranceDecisionFSM :: PeekFSM 'PeekInsuranceDecision
  PeekInsuranceSettledFSM :: PeekFSM 'PeekInsuranceSettled
  PeekPlayersFSM :: PeekFSM 'PeekPlayers
  PeekDealerFSM :: PeekFSM 'PeekDealer
  PeekSettleFSM :: PeekFSM 'PeekSettle
  PeekCompleteFSM :: PeekFSM 'PeekComplete

deriving instance Show (PeekFSM p)

deriving instance Eq (PeekFSM p)

data PeekPhase
  = PeekAwaiting
  | PeekBets
  | PeekDeal
  | PeekEarlySurrender
  | PeekPeek
  | PeekInsuranceDecision
  | PeekInsuranceSettled
  | PeekPlayers
  | PeekDealer
  | PeekSettle
  | PeekComplete

instance PhaseTag PeekFSM DealerRoundPhase where
  phaseTag = \case
    PeekAwaitingFSM -> Awaiting
    PeekBetsFSM -> Bets
    PeekDealFSM -> Deal
    PeekEarlySurrenderFSM -> EarlySurrender
    PeekPeekFSM -> Peek
    PeekInsuranceDecisionFSM -> InsuranceDecision
    PeekInsuranceSettledFSM -> InsuranceSettled
    PeekPlayersFSM -> Players
    PeekDealerFSM -> Dealer
    PeekSettleFSM -> Settle
    PeekCompleteFSM -> Complete

instance Transitionable (PeekFSM p) where
  transitionType = \case
    PeekAwaitingFSM -> AwaitInput
    PeekBetsFSM -> AwaitInput
    PeekDealFSM -> AutoAdvance
    PeekEarlySurrenderFSM -> AwaitInput
    PeekPeekFSM -> AwaitInput
    PeekInsuranceDecisionFSM -> AwaitInput
    PeekInsuranceSettledFSM -> AutoAdvance
    PeekPlayersFSM -> AwaitInput
    PeekDealerFSM -> AutoAdvance
    PeekSettleFSM -> AutoAdvance
    PeekCompleteFSM -> TerminalPhase

data SomePeekFSM = forall p. SomePeekFSM (PeekFSM p)

instance Transitionable SomePeekFSM where
  transitionType (SomePeekFSM fsm) = transitionType fsm

instance Show SomePeekFSM where
  show (SomePeekFSM fsm) = show fsm

instance Eq SomePeekFSM where
  SomePeekFSM a == SomePeekFSM b = case (a, b) of
    (PeekAwaitingFSM, PeekAwaitingFSM) -> True
    (PeekBetsFSM, PeekBetsFSM) -> True
    (PeekDealFSM, PeekDealFSM) -> True
    (PeekEarlySurrenderFSM, PeekEarlySurrenderFSM) -> True
    (PeekPeekFSM, PeekPeekFSM) -> True
    (PeekInsuranceDecisionFSM, PeekInsuranceDecisionFSM) -> True
    (PeekInsuranceSettledFSM, PeekInsuranceSettledFSM) -> True
    (PeekPlayersFSM, PeekPlayersFSM) -> True
    (PeekDealerFSM, PeekDealerFSM) -> True
    (PeekSettleFSM, PeekSettleFSM) -> True
    (PeekCompleteFSM, PeekCompleteFSM) -> True
    _ -> False

instance ToJSON SomePeekFSM where
  toJSON (SomePeekFSM fsm) = case fsm of
    PeekAwaitingFSM -> object ["tag" .= String "PeekAwaiting"]
    PeekBetsFSM -> object ["tag" .= String "PeekBets"]
    PeekDealFSM -> object ["tag" .= String "PeekDeal"]
    PeekEarlySurrenderFSM -> object ["tag" .= String "PeekEarlySurrender"]
    PeekPeekFSM -> object ["tag" .= String "PeekPeek"]
    PeekInsuranceDecisionFSM -> object ["tag" .= String "PeekInsuranceDecision"]
    PeekInsuranceSettledFSM -> object ["tag" .= String "PeekInsuranceSettled"]
    PeekPlayersFSM -> object ["tag" .= String "PeekPlayers"]
    PeekDealerFSM -> object ["tag" .= String "PeekDealer"]
    PeekSettleFSM -> object ["tag" .= String "PeekSettle"]
    PeekCompleteFSM -> object ["tag" .= String "PeekComplete"]

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
      "PeekPlayers" -> pure $ SomePeekFSM PeekPlayersFSM
      "PeekDealer" -> pure $ SomePeekFSM PeekDealerFSM
      "PeekSettle" -> pure $ SomePeekFSM PeekSettleFSM
      "PeekComplete" -> pure $ SomePeekFSM PeekCompleteFSM
      _ -> fail $ "Unknown PeekFSM tag: " ++ tag
