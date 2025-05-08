{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pitboss.FSM.DealerRoundFSM.JSON where

import Data.Aeson
import Data.Text qualified as T
import Pitboss.FSM.DealerRoundFSM.Existential
import Pitboss.FSM.DealerRoundFSM.Types

instance ToJSON InterruptReason

instance FromJSON InterruptReason

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
      "ENHCDealer" -> pure $ SomeENHCFSM ENHCDealerFSM
      "ENHCSettle" -> pure $ SomeENHCFSM ENHCSettleFSM
      "ENHCComplete" -> pure $ SomeENHCFSM ENHCCompleteFSM
      _ -> fail $ "Unknown ENHCFSM tag: " ++ tag

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
      "PeekPlayers" -> pure $ SomePeekFSM PeekPlayersFSM
      "PeekDealer" -> pure $ SomePeekFSM PeekDealerFSM
      "PeekSettle" -> pure $ SomePeekFSM PeekSettleFSM
      "PeekComplete" -> pure $ SomePeekFSM PeekCompleteFSM
      _ -> fail $ "Unknown PeekFSM tag: " ++ tag
