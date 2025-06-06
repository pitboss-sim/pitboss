{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.PlayerHand where

import Pitboss.Blackjack
import Pitboss.FSM.Types

type family ValidPlayerHandTransition (from :: PlayerHandPhase) (to :: PlayerHandPhase) :: Bool where
    ValidPlayerHandTransition 'PHDecision 'PHHitting = 'True
    ValidPlayerHandTransition 'PHDecision ('PHAwaitingOneCard reason) = 'True
    ValidPlayerHandTransition 'PHDecision ('PHResolved res) = 'True
    ValidPlayerHandTransition 'PHHitting 'PHHitting = 'True
    ValidPlayerHandTransition 'PHHitting ('PHResolved 'PHBust) = 'True
    ValidPlayerHandTransition 'PHHitting ('PHResolved 'PHStand) = 'True
    ValidPlayerHandTransition ('PHAwaitingOneCard reason) ('PHResolved res) = 'True
    ValidPlayerHandTransition _ _ = 'False

-- resolutionImpact :: PlayerHandResolution -> Maybe BankrollImpact
-- resolutionImpact = \case
--     PHSurrendered -> Just Refund
--     PHPush -> Just Refund
--     PHBust -> Just Loss
--     PHDealerBlackjack -> Just Loss
--     PHVoid i -> Just i
--     _ -> Nothing

initialDecision :: SomeHand -> PlayerHandFSM 'PHDecision 'OKHit 'OKDbl 'OKSpl
initialDecision _hand = PHDecisionFSM

resolveSurrender ::
    SomeHand ->
    GameRuleSet ->
    PlayerHandFSM 'PHDecision h d s ->
    Either String (PlayerHandFSM ('PHResolved 'PHSurrendered) 'NoHit 'NoDbl 'NoSpl)
resolveSurrender hand rules PHDecisionFSM = do
    validateMoveInContext MSurrender hand rules 0
    Right (PHResolvedFSM PHSurrendered)

toOneCardDrawFromDecision ::
    OneCardDrawReason ->
    SomeHand ->
    GameRuleSet ->
    Int ->
    PlayerHandFSM 'PHDecision 'OKHit d s ->
    Either String (PlayerHandFSM ('PHAwaitingOneCard reason) 'NoHit 'NoDbl 'NoSpl)
toOneCardDrawFromDecision reason hand rules splitCount PHDecisionFSM = do
    let move = case reason of
            OCDouble -> MDouble
            OCSplitAce -> MSplit
            OCSplitNonAce -> MSplit
    validateMoveInContext move hand rules splitCount
    Right (PHAwaitingOneCardFSM reason)

toHitting ::
    SomeHand ->
    PlayerHandFSM 'PHDecision 'OKHit d s ->
    Either String (PlayerHandFSM 'PHHitting 'OKHit d s)
toHitting hand PHDecisionFSM = do
    if isLegalMove MHit hand undefined
        then Right PHHittingFSM
        else Left "Cannot hit: hand cannot take another card"

continueHitting ::
    SomeHand ->
    PlayerHandFSM 'PHHitting h d s ->
    Either String (PlayerHandFSM 'PHHitting h d s)
continueHitting hand PHHittingFSM = do
    if isLegalMove MHit hand undefined
        then Right PHHittingFSM
        else Left "Cannot continue hitting: hand cannot take another card"

resolveStand ::
    SomeHand ->
    PlayerHandFSM 'PHDecision h d s ->
    Either String (PlayerHandFSM ('PHResolved 'PHStand) 'NoHit 'NoDbl 'NoSpl)
resolveStand _hand PHDecisionFSM = Right (PHResolvedFSM PHStand)

resolveBust ::
    SomeHand ->
    PlayerHandFSM 'PHHitting h d s ->
    Either String (PlayerHandFSM ('PHResolved 'PHBust) 'NoHit 'NoDbl 'NoSpl)
resolveBust hand PHHittingFSM =
    let witness = handWitness hand
     in if valueType witness == BustWitness
            then Right (PHResolvedFSM PHBust)
            else Left "Cannot resolve as bust: hand is not busted"

resolveOneCardDraw ::
    SomeHand ->
    PlayerHandResolution ->
    PlayerHandFSM ('PHAwaitingOneCard reason) 'NoHit 'NoDbl 'NoSpl ->
    Either String (PlayerHandFSM ('PHResolved res) 'NoHit 'NoDbl 'NoSpl)
resolveOneCardDraw hand res (PHAwaitingOneCardFSM _) =
    let witness = handWitness hand
        actualResolution = case valueType witness of
            BustWitness -> PHBust
            BlackjackWitness -> PHBlackjack
            _ -> PHStand
     in if actualResolution == res
            then Right (PHResolvedFSM res)
            else Left $ "Resolution mismatch: expected " ++ show res ++ ", hand shows " ++ show actualResolution

resolveSplit ::
    SomeHand ->
    GameRuleSet ->
    Int ->
    PlayerHandFSM 'PHDecision h d s ->
    Either String (PlayerHandFSM ('PHResolved res) 'NoHit 'NoDbl 'NoSpl)
resolveSplit hand rules splitCount PHDecisionFSM = do
    validateMoveInContext MSplit hand rules splitCount
    let witness = handWitness hand
    case structure witness of
        PairWitness Ace -> Right (PHResolvedFSM PHSplitAces)
        PairWitness _ -> Right (PHResolvedFSM PHSplitNonAces)
        _ -> Left "Cannot split: hand is not a pair"

resolvePush ::
    SomeHand ->
    PlayerHandFSM from h d s ->
    PlayerHandFSM ('PHResolved 'PHPush) 'NoHit 'NoDbl 'NoSpl
resolvePush _hand _ = PHResolvedFSM PHPush

resolveDealerBlackjack ::
    SomeHand ->
    PlayerHandFSM from h d s ->
    PlayerHandFSM ('PHResolved 'PHDealerBlackjack) 'NoHit 'NoDbl 'NoSpl
resolveDealerBlackjack _hand _ = PHResolvedFSM PHDealerBlackjack

resolveVoid ::
    SomeHand ->
    BankrollImpact ->
    PlayerHandFSM from h d s ->
    PlayerHandFSM ('PHResolved ('PHVoid impact)) 'NoHit 'NoDbl 'NoSpl
resolveVoid _hand impact _ = PHResolvedFSM (PHVoid impact)
