{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Bout where

import Pitboss.FSM.Types

type family ValidBoutTransition (from :: BoutPhase) (to :: BoutPhase) :: Bool where
    ValidBoutTransition 'BAwaitingFirstCard 'BAwaitingSecondCard = 'True
    ValidBoutTransition 'BAwaitingSecondCard 'BPlayerTurn = 'True
    ValidBoutTransition 'BPlayerTurn 'BDealerTurn = 'True
    ValidBoutTransition 'BPlayerTurn 'BSettlement = 'True
    ValidBoutTransition 'BDealerTurn 'BSettlement = 'True
    ValidBoutTransition 'BSettlement 'BDone = 'True
    ValidBoutTransition _ _ = 'False

dealFirstCard ::
    BoutFSM 'BAwaitingFirstCard ->
    BoutFSM 'BAwaitingSecondCard
dealFirstCard BAwaitingFirstCardFSM = BAwaitingSecondCardFSM

dealSecondCard ::
    BoutFSM 'BAwaitingSecondCard ->
    BoutFSM 'BPlayerTurn
dealSecondCard BAwaitingSecondCardFSM = BPlayerTurnFSM

playerComplete ::
    BoutFSM 'BPlayerTurn ->
    BoutFSM 'BDealerTurn
playerComplete BPlayerTurnFSM = BDealerTurnFSM

playerBustOrBlackjack ::
    BoutFSM 'BPlayerTurn ->
    BoutFSM 'BSettlement
playerBustOrBlackjack BPlayerTurnFSM = BSettlementFSM

boutDealerComplete ::
    BoutFSM 'BDealerTurn ->
    BoutFSM 'BSettlement
boutDealerComplete BDealerTurnFSM = BSettlementFSM

settleOutcome ::
    BoutFSM 'BSettlement ->
    BoutFSM 'BDone
settleOutcome BSettlementFSM = BDoneFSM
