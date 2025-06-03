{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Sim.Agency.Intent.Instances.Validateable where

import Pitboss.Sim.Agency.Intent.Types
import Pitboss.Blackjack
import Pitboss.FSM
import Pitboss.FSM.Instances.Transitionable
import Pitboss.State.Types.Core

class Validateable (k :: IntentKind) where
    validate :: IntentCtx k -> Either String ()

instance Validateable 'IPlayerHit where
    validate ctx = do
        checkIsPlayersTurn ctx
        checkHandNotBusted ctx
        checkCanMakeDecision ctx
        checkShoeHasCards ctx
        checkNotAt21 ctx
      where
        checkIsPlayersTurn :: IntentCtx 'IPlayerHit -> Either String ()
        checkIsPlayersTurn ctx' =
            if phICtxIsPlayersTurn ctx'
                then Right ()
                else Left "Not player's turn"

        checkHandNotBusted :: IntentCtx 'IPlayerHit -> Either String ()
        checkHandNotBusted ctx' =
            if not (isBusted (phICtxPlayerHand ctx'))
                then Right ()
                else Left "Cannot hit on busted hand"

        checkCanMakeDecision :: IntentCtx 'IPlayerHit -> Either String ()
        checkCanMakeDecision ctx' = case phICtxPlayerHandFSM ctx' of
            SomePlayerHandFSM PHDecisionFSM -> Right ()
            SomePlayerHandFSM PHHittingFSM -> Right ()
            _ -> Left "Hand not in a state where hitting is allowed"

        checkShoeHasCards :: IntentCtx 'IPlayerHit -> Either String ()
        checkShoeHasCards ctx' =
            if phICtxShoeHasCards ctx'
                then Right ()
                else Left "No cards available in shoe"

        checkNotAt21 :: IntentCtx 'IPlayerHit -> Either String ()
        checkNotAt21 ctx' =
            if handScore (phICtxPlayerHand ctx') < 21
                then Right ()
                else Left "Cannot hit when hand totals 21"

instance Validateable 'IPlayerStand where
    validate ctx = do
        checkIsPlayersTurn ctx
        checkHandNotBusted ctx
        checkCanMakeDecision ctx
      where
        checkIsPlayersTurn :: IntentCtx 'IPlayerStand -> Either String ()
        checkIsPlayersTurn ctx' =
            if psICtxIsPlayersTurn ctx'
                then Right ()
                else Left "Not player's turn"

        checkHandNotBusted :: IntentCtx 'IPlayerStand -> Either String ()
        checkHandNotBusted ctx' =
            if not (isBusted (psICtxPlayerHand ctx'))
                then Right ()
                else Left "Cannot stand on busted hand"

        checkCanMakeDecision :: IntentCtx 'IPlayerStand -> Either String ()
        checkCanMakeDecision ctx' = case psICtxPlayerHandFSM ctx' of
            SomePlayerHandFSM PHDecisionFSM -> Right ()
            SomePlayerHandFSM PHHittingFSM -> Right ()
            _ -> Left "Hand not in a state where standing is allowed"

instance Validateable 'IPlayerDouble where
    validate ctx = do
        checkIsPlayersTurn ctx
        checkCanDouble ctx
        checkHasFundsToDouble ctx
        checkCanMakeDecision ctx
      where
        checkIsPlayersTurn :: IntentCtx 'IPlayerDouble -> Either String ()
        checkIsPlayersTurn ctx' =
            if pdICtxIsPlayersTurn ctx'
                then Right ()
                else Left "Not player's turn"

        checkCanDouble :: IntentCtx 'IPlayerDouble -> Either String ()
        checkCanDouble ctx' =
            if canDoubleSomeHand (pdICtxPlayerHand ctx') (pdICtxOffering ctx')
                then Right ()
                else Left "Cannot double this hand per table rules"

        checkHasFundsToDouble :: IntentCtx 'IPlayerDouble -> Either String ()
        checkHasFundsToDouble ctx' =
            let
                (Chips bankroll) = pdICtxPlayerBankroll ctx'
                (Chips bet) = pdICtxCurrentBet ctx'
             in
                if bankroll >= bet
                    then Right ()
                    else Left "Insufficient funds to double"

        checkCanMakeDecision :: IntentCtx 'IPlayerDouble -> Either String ()
        checkCanMakeDecision ctx' = case pdICtxPlayerHandFSM ctx' of
            SomePlayerHandFSM PHDecisionFSM -> Right ()
            _ -> Left "Hand not in initial decision state"

instance Validateable 'IPlayerSplit where
    validate _ = Left "Player split not yet implemented"

instance Validateable 'IPlayerSurrender where
    validate _ = Left "Player surrender not yet implemented"

instance Validateable 'IDealerHit where
    validate ctx = do
        checkDealerMustHit ctx
        checkInDealingPhase ctx
      where
        checkDealerMustHit :: IntentCtx 'IDealerHit -> Either String ()
        checkDealerMustHit ctx' =
            if dealerShouldHit (dhICtxGameRules ctx') (dhICtxDealerHand ctx')
                then Right ()
                else Left "Dealer should not hit with this hand"

        checkInDealingPhase :: IntentCtx 'IDealerHit -> Either String ()
        checkInDealingPhase ctx' = case dhICtxDealerHandFSM ctx' of
            SomeDealerHandFSM DHDealingFSM -> Right ()
            _ -> Left "Dealer hand not in dealing phase"

instance Validateable 'IDealerStand where
    validate ctx = do
        checkDealerMustStand ctx
        checkInDealingPhase ctx
      where
        checkDealerMustStand :: IntentCtx 'IDealerStand -> Either String ()
        checkDealerMustStand ctx' =
            if not (dealerShouldHit (dsICtxGameRules ctx') (dsICtxDealerHand ctx'))
                then Right ()
                else Left "Dealer must hit with this hand"

        checkInDealingPhase :: IntentCtx 'IDealerStand -> Either String ()
        checkInDealingPhase ctx' = case dsICtxDealerHandFSM ctx' of
            SomeDealerHandFSM DHDealingFSM -> Right ()
            _ -> Left "Dealer hand not in dealing phase"

instance Validateable 'IDealerDeal where
    validate ctx = do
        checkShoeHasCards ctx
        checkTargetCanReceiveCard ctx
        checkRoundPhaseAllowsDealing ctx
      where
        checkShoeHasCards :: IntentCtx 'IDealerDeal -> Either String ()
        checkShoeHasCards ctx' =
            if ddICtxShoeCardsRemaining ctx' > 0
                then Right ()
                else Left "No cards remaining in shoe"

        checkTargetCanReceiveCard :: IntentCtx 'IDealerDeal -> Either String ()
        checkTargetCanReceiveCard ctx' = case ddICtxTargetHandFSM ctx' of
            Left playerFSM ->
                if not (isHandTerminal playerFSM)
                    then Right ()
                    else Left "Player hand cannot receive card - already complete"
            Right _dealerFSM ->
                Right ()

        checkRoundPhaseAllowsDealing :: IntentCtx 'IDealerDeal -> Either String ()
        checkRoundPhaseAllowsDealing _ = Right ()

        isHandTerminal :: SomePlayerHandFSM -> Bool
        isHandTerminal (SomePlayerHandFSM fsm) =
            case transitionType fsm of
                TerminalPhase -> True
                _ -> False

instance Validateable 'IDealerSettleBout where
    validate _ = Left "Dealer settle bout not yet implemented"

instance Validateable 'IDealerSettleInsurance where
    validate _ = Left "Dealer settle insurance not yet implemented"
