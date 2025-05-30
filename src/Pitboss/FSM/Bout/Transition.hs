{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.Bout.Transition where

import Pitboss.FSM.Bout.FSM
import Pitboss.FSM.Bout.Phase

type family ValidBoutTransition (from :: BoutPhase) (to :: BoutPhase) :: Bool where
    ValidBoutTransition 'BAwaitingFirstCard 'BAwaitingSecondCard = 'True
    ValidBoutTransition 'BAwaitingSecondCard 'BPlayerTurn = 'True
    ValidBoutTransition 'BPlayerTurn 'BDealerTurn = 'True
    ValidBoutTransition 'BPlayerTurn 'BSettlement = 'True
    ValidBoutTransition 'BDealerTurn 'BSettlement = 'True
    ValidBoutTransition 'BSettlement 'BDone = 'True
    ValidBoutTransition _ _ = 'False

dealFirstCard ::
    (ValidBoutTransition 'BAwaitingFirstCard 'BAwaitingSecondCard ~ 'True) =>
    BoutFSM 'BAwaitingFirstCard ->
    BoutFSM 'BAwaitingSecondCard
dealFirstCard BAwaitingFirstCardFSM = BAwaitingSecondCardFSM

dealSecondCard ::
    (ValidBoutTransition 'BAwaitingSecondCard 'BPlayerTurn ~ 'True) =>
    BoutFSM 'BAwaitingSecondCard ->
    BoutFSM 'BPlayerTurn
dealSecondCard BAwaitingSecondCardFSM = BPlayerTurnFSM

playerComplete ::
    (ValidBoutTransition 'BPlayerTurn 'BDealerTurn ~ 'True) =>
    BoutFSM 'BPlayerTurn ->
    BoutFSM 'BDealerTurn
playerComplete BPlayerTurnFSM = BDealerTurnFSM

playerBustOrBlackjack ::
    (ValidBoutTransition 'BPlayerTurn 'BSettlement ~ 'True) =>
    BoutFSM 'BPlayerTurn ->
    BoutFSM 'BSettlement
playerBustOrBlackjack BPlayerTurnFSM = BSettlementFSM

dealerComplete ::
    (ValidBoutTransition 'BDealerTurn 'BSettlement ~ 'True) =>
    BoutFSM 'BDealerTurn ->
    BoutFSM 'BSettlement
dealerComplete BDealerTurnFSM = BSettlementFSM

settleOutcome ::
    (ValidBoutTransition 'BSettlement 'BDone ~ 'True) =>
    BoutFSM 'BSettlement ->
    BoutFSM 'BDone
settleOutcome BSettlementFSM = BDoneFSM
