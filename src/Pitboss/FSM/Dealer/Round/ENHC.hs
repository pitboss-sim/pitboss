{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.Dealer.Round.ENHC (
    module Pitboss.FSM.Dealer.Round.ENHC.FSM,
    module Pitboss.FSM.Dealer.Round.ENHC.Phase,
    module Pitboss.FSM.Dealer.Round.ENHC.Transition,
    SomeENHCFSM (..),
)
where

import Data.Aeson
import Pitboss.FSM.Dealer.Round.ENHC.FSM
import Pitboss.FSM.Dealer.Round.ENHC.Phase
import Pitboss.FSM.Dealer.Round.ENHC.Transition

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
