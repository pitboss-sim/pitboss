{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.Bout.Transition where

import Pitboss.FSM.Bout.FSM
import Pitboss.FSM.Bout.Phase

type family ValidBoutTransition (from :: BoutPhase) (to :: BoutPhase) :: Bool where
    ValidBoutTransition 'AwaitingFirstCard 'AwaitingSecondCard = 'True
    ValidBoutTransition 'AwaitingSecondCard 'PlayerTurn = 'True
    ValidBoutTransition 'PlayerTurn 'DealerTurn = 'True
    ValidBoutTransition 'PlayerTurn 'Settlement = 'True
    ValidBoutTransition 'DealerTurn 'Settlement = 'True
    ValidBoutTransition 'Settlement 'Done = 'True
    ValidBoutTransition _ _ = 'False

dealFirstCard ::
    (ValidBoutTransition 'AwaitingFirstCard 'AwaitingSecondCard ~ 'True) =>
    BoutFSM 'AwaitingFirstCard ->
    BoutFSM 'AwaitingSecondCard
dealFirstCard AwaitingFirstCardFSM = AwaitingSecondCardFSM

dealSecondCard ::
    (ValidBoutTransition 'AwaitingSecondCard 'PlayerTurn ~ 'True) =>
    BoutFSM 'AwaitingSecondCard ->
    BoutFSM 'PlayerTurn
dealSecondCard AwaitingSecondCardFSM = PlayerTurnFSM

playerComplete ::
    (ValidBoutTransition 'PlayerTurn 'DealerTurn ~ 'True) =>
    BoutFSM 'PlayerTurn ->
    BoutFSM 'DealerTurn
playerComplete PlayerTurnFSM = DealerTurnFSM

playerBustOrBlackjack ::
    (ValidBoutTransition 'PlayerTurn 'Settlement ~ 'True) =>
    BoutFSM 'PlayerTurn ->
    BoutFSM 'Settlement
playerBustOrBlackjack PlayerTurnFSM = SettlementFSM

dealerComplete ::
    (ValidBoutTransition 'DealerTurn 'Settlement ~ 'True) =>
    BoutFSM 'DealerTurn ->
    BoutFSM 'Settlement
dealerComplete DealerTurnFSM = SettlementFSM

settleOutcome ::
    (ValidBoutTransition 'Settlement 'Done ~ 'True) =>
    BoutFSM 'Settlement ->
    BoutFSM 'Done
settleOutcome SettlementFSM = DoneFSM
