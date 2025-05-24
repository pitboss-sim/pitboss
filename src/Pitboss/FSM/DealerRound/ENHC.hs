{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Pitboss.FSM.DealerRound.ENHC (
    module Pitboss.FSM.DealerRound.ENHC.FSM,
    module Pitboss.FSM.DealerRound.ENHC.Phase,
    module Pitboss.FSM.DealerRound.ENHC.Transition,
    SomeENHCFSM (..),
    mkSomeENHCAwaiting,
    mkSomeENHCBets,
    mkSomeENHCDeal,
    mkSomeENHCPlayers,
    mkSomeENHCDealing,
    mkSomeENHCSettle,
    mkSomeENHCComplete,
)
where

import Data.Aeson
import Pitboss.FSM.DealerRound.ENHC.FSM
import Pitboss.FSM.DealerRound.ENHC.Phase
import Pitboss.FSM.DealerRound.ENHC.Transition
import Pitboss.FSM.DealerRound.Phase
import Pitboss.FSM.Types.Transitionable

data SomeENHCFSM = forall p. SomeENHCFSM (ENHCFSM p)

mkSomeENHCAwaiting :: SomeENHCFSM
mkSomeENHCAwaiting = SomeENHCFSM ENHCAwaitingFSM

mkSomeENHCBets :: SomeENHCFSM
mkSomeENHCBets = SomeENHCFSM ENHCBetsFSM

mkSomeENHCDeal :: SomeENHCFSM
mkSomeENHCDeal = SomeENHCFSM ENHCDealFSM

mkSomeENHCEarlySurrender :: SomeENHCFSM
mkSomeENHCEarlySurrender = SomeENHCFSM ENHCEarlySurrenderFSM

mkSomeENHCPlayers :: SomeENHCFSM
mkSomeENHCPlayers = SomeENHCFSM ENHCPlayersFSM

mkSomeENHCDealing :: SomeENHCFSM
mkSomeENHCDealing = SomeENHCFSM ENHCDealingFSM

mkSomeENHCSettle :: SomeENHCFSM
mkSomeENHCSettle = SomeENHCFSM ENHCSettleFSM

mkSomeENHCComplete :: SomeENHCFSM
mkSomeENHCComplete = SomeENHCFSM ENHCCompleteFSM

mkSomeENHCInterrupted :: InterruptReason -> SomeENHCFSM
mkSomeENHCInterrupted r = SomeENHCFSM (ENHCInterruptedFSM r)

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
            "ENHCEarlySurrender" -> pure mkSomeENHCEarlySurrender
            "ENHCPlayers" -> pure $ SomeENHCFSM ENHCPlayersFSM
            "ENHCDealing" -> pure $ SomeENHCFSM ENHCDealingFSM
            "ENHCSettle" -> pure $ SomeENHCFSM ENHCSettleFSM
            "ENHCComplete" -> pure $ SomeENHCFSM ENHCCompleteFSM
            _ -> fail $ "Unknown ENHCFSM tag: " ++ tag
