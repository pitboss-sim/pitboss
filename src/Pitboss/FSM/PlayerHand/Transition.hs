{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.PlayerHand.Transition where

import Pitboss.Blackjack hiding (SplitAces, Push, Stand, HandPhase)
import Pitboss.FSM.PlayerHand.FSM
import Pitboss.FSM.PlayerHand.Phase

type family ValidPlayerHandTransition (from :: HandPhase) (to :: HandPhase) :: Bool where
    ValidPlayerHandTransition 'PHDecision 'PHHitting = 'True
    ValidPlayerHandTransition 'PHDecision ('PHOneCardDraw reason) = 'True
    ValidPlayerHandTransition 'PHDecision ('PHResolved res) = 'True
    ValidPlayerHandTransition 'PHHitting 'PHHitting = 'True
    ValidPlayerHandTransition 'PHHitting ('PHResolved 'PHBust) = 'True
    ValidPlayerHandTransition 'PHHitting ('PHResolved 'PHStand) = 'True
    ValidPlayerHandTransition ('PHOneCardDraw reason) ('PHResolved res) = 'True
    ValidPlayerHandTransition _ _ = 'False

initialDecisionTyped :: SomeHand -> PlayerHandFSM 'PHDecision 'OKHit 'OKDbl 'OKSpl
initialDecisionTyped _hand = PHDecisionFSM

resolveSurrenderTyped ::
    (ValidPlayerHandTransition 'PHDecision ('PHResolved 'PHSurrendered) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'PHDecision h d s ->
    PlayerHandFSM ('PHResolved 'PHSurrendered) 'NoHit 'NoDbl 'NoSpl
resolveSurrenderTyped _hand PHDecisionFSM = PHResolvedFSM PHSurrendered

toOneCardDrawFromDecisionTyped ::
    (ValidPlayerHandTransition 'PHDecision ('PHOneCardDraw reason) ~ 'True) =>
    OneCardDrawReason ->
    SomeHand ->
    PlayerHandFSM 'PHDecision 'OKHit d s ->
    PlayerHandFSM ('PHOneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
toOneCardDrawFromDecisionTyped reason _hand PHDecisionFSM = PHOneCardDrawFSM reason

toHittingTyped ::
    (ValidPlayerHandTransition 'PHDecision 'PHHitting ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'PHDecision 'OKHit d s ->
    PlayerHandFSM 'PHHitting 'OKHit d s
toHittingTyped _hand PHDecisionFSM = PHHittingFSM

continueHittingTyped ::
    (ValidPlayerHandTransition 'PHHitting 'PHHitting ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'PHHitting h d s ->
    PlayerHandFSM 'PHHitting h d s
continueHittingTyped _hand PHHittingFSM = PHHittingFSM

resolveStandTyped ::
    (ValidPlayerHandTransition 'PHDecision ('PHResolved 'PHStand) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'PHDecision h d s ->
    PlayerHandFSM ('PHResolved 'PHStand) 'NoHit 'NoDbl 'NoSpl
resolveStandTyped _hand PHDecisionFSM = PHResolvedFSM PHStand

resolveBustTyped ::
    (ValidPlayerHandTransition 'PHHitting ('PHResolved 'PHBust) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'PHHitting h d s ->
    PlayerHandFSM ('PHResolved 'PHBust) 'NoHit 'NoDbl 'NoSpl
resolveBustTyped (SomeHand hand) PHHittingFSM = case witness hand of
    BustWitness -> PHResolvedFSM PHBust
    _ -> error "resolveBustTyped: hand is not busted"

resolveOneCardDrawTyped ::
    (ValidPlayerHandTransition ('PHOneCardDraw reason) ('PHResolved res) ~ 'True) =>
    SomeHand ->
    PlayerHandResolution ->
    PlayerHandFSM ('PHOneCardDraw reason) 'NoHit 'NoDbl 'NoSpl ->
    PlayerHandFSM ('PHResolved res) 'NoHit 'NoDbl 'NoSpl
resolveOneCardDrawTyped (SomeHand hand) res (PHOneCardDrawFSM _) = case (witness hand, res) of
    (BustWitness, PHBust) -> PHResolvedFSM res
    (BlackjackWitness, PHBlackjack) -> PHResolvedFSM res
    (_, PHStand) | handScore (SomeHand hand) <= 21 -> PHResolvedFSM res
    _ -> error "resolveOneCardDrawTyped: invalid resolution for hand"

resolveSplitTyped ::
    (ValidPlayerHandTransition 'PHDecision ('PHResolved res) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'PHDecision h d s ->
    Offering ->
    PlayerHandFSM ('PHResolved res) 'NoHit 'NoDbl 'NoSpl
resolveSplitTyped someHand PHDecisionFSM offering =
    case fromSomeHand someHand of
        SomeLifecycleHand fullHand@(FullLifecycleHand _) ->
            if canSplitHand fullHand 0 offering
                then case extractPairRank someHand of
                    Just Ace -> PHResolvedFSM PHSplitAces
                    Just _ -> PHResolvedFSM PHSplitNonAces
                    Nothing -> error "resolveSplitTyped: not a pair"
                else error "resolveSplitTyped: hand cannot be split"
        _ -> error "resolveSplitTyped: not a full hand"

resolvePushTyped ::
    (ValidPlayerHandTransition from ('PHResolved 'PHPush) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM from h d s ->
    PlayerHandFSM ('PHResolved 'PHPush) 'NoHit 'NoDbl 'NoSpl
resolvePushTyped _hand _ = PHResolvedFSM PHPush

resolveDealerBlackjackTyped ::
    (ValidPlayerHandTransition from ('PHResolved 'PHDealerBlackjack) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM from h d s ->
    PlayerHandFSM ('PHResolved 'PHDealerBlackjack) 'NoHit 'NoDbl 'NoSpl
resolveDealerBlackjackTyped _hand _ = PHResolvedFSM PHDealerBlackjack

resolveVoidTyped ::
    (ValidPlayerHandTransition from ('PHResolved ('PHVoid impact)) ~ 'True) =>
    SomeHand ->
    BankrollImpact ->
    PlayerHandFSM from h d s ->
    PlayerHandFSM ('PHResolved ('PHVoid impact)) 'NoHit 'NoDbl 'NoSpl
resolveVoidTyped _hand impact _ = PHResolvedFSM (PHVoid impact)
