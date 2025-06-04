{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Simulation.Intent.ValidationContext where

import Control.Monad.Reader
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Causality.Validate
import Pitboss.Simulation.Intent.Types

type family IntentValidationData (k :: IntentKind) where
    IntentValidationData 'IPlayerHit = (ContestantId, HandId, BoutId, ShoeId, SomeHand, Bool)
    IntentValidationData 'IPlayerStand = (ContestantId, HandId, BoutId, SomeHand)
    IntentValidationData 'IPlayerDouble = (ContestantId, HandId, BoutId, PlayerId, SomeHand, Chips, Offering)
    IntentValidationData 'IPlayerSplit = (ContestantId, HandId, BoutId, PlayerId, SomeHand, Chips, Int)

class ValidatableIntent k where
    validateIntent :: IntentCtx k -> ValidatedReader (IntentValidationData k)

validateSplitOperation :: ContestantId -> HandId -> Reader TickCacheContext (Validation ValidationErrors (EntityState 'Contestant, EntityState 'Hand, EntityState 'Bout))
validateSplitOperation contestantId handId = do
    contestantV <- derefV contestantId
    handV <- derefV handId

    case handV of
        Success hand -> do
            let boutId = _hRelsBout (_hRels hand)
            boutV <- derefV boutId
            pure $ validateOwnership contestantId <$> contestantV <*> handV <*> boutV
        Failure e -> pure (Failure e)
  where
    validateOwnership cid contestant hand bout =
        case _hRelsOwner (_hRels hand) of
            ContestantOwner ownerId | ownerId == cid -> (contestant, hand, bout)
            ContestantOwner ownerId -> error $ "Hand owned by different contestant: " ++ show ownerId
            DealerOwner _ -> error "Hand owned by dealer, not contestant"

validateMultipleEntities :: ContestantId -> HandId -> BoutId -> Reader TickCacheContext (Validation ValidationErrors (EntityState 'Contestant, EntityState 'Hand, EntityState 'Bout))
validateMultipleEntities contestantId handId boutId = do
    contestantV <- derefV contestantId
    handV <- derefV handId
    boutV <- derefV boutId
    pure $ (,,) <$> contestantV <*> handV <*> boutV

instance ValidatableIntent 'IPlayerHit where
    validateIntent ctx = do
        handV <- derefV (phICtxHandId ctx)
        contestantV <- derefV (phICtxContestantId ctx)
        return $ validatePlayerHitData <$> handV <*> contestantV
      where
        validatePlayerHitData hand contestant =
            let shoeId = _cRelsShoe (_cRels contestant)
                handData = _hAttrsHand (_hAttrs hand)
                canHit' = handScore handData < 21 && not (isBust (handProperties handData))
                boutId = extractActiveBout contestant
             in (phICtxContestantId ctx, phICtxHandId ctx, boutId, shoeId, handData, canHit')

        extractActiveBout contestant =
            case lookupFiniteMap Hand1 (_cRelsBouts (_cRels contestant)) of
                Just (Present boutId) -> boutId
                _ -> error "No active bout found"

instance ValidatableIntent 'IPlayerStand where
    validateIntent ctx = do
        handV <- derefV (psICtxHandId ctx)
        contestantV <- derefV (psICtxContestantId ctx)
        return $ validatePlayerStandData <$> handV <*> contestantV
      where
        validatePlayerStandData hand contestant =
            let handData = _hAttrsHand (_hAttrs hand)
                boutId = extractActiveBout contestant
             in (psICtxContestantId ctx, psICtxHandId ctx, boutId, handData)

        extractActiveBout contestant =
            case lookupFiniteMap Hand1 (_cRelsBouts (_cRels contestant)) of
                Just (Present boutId) -> boutId
                _ -> error "No active bout found"

instance ValidatableIntent 'IPlayerDouble where
    validateIntent ctx = do
        handV <- derefV (pdICtxHandId ctx)
        contestantV <- derefV (pdICtxContestantId ctx)
        playerV <- derefV (pdICtxPlayerId ctx)
        return $ validatePlayerDoubleData <$> handV <*> contestantV <*> playerV
      where
        validatePlayerDoubleData hand contestant player =
            let handData = _hAttrsHand (_hAttrs hand)
                bankroll = _pAttrsBankroll (_pAttrs player)
                offering = vegas6
                boutId = extractActiveBout contestant
             in (pdICtxContestantId ctx, pdICtxHandId ctx, boutId, pdICtxPlayerId ctx, handData, bankroll, offering)

        extractActiveBout contestant =
            case lookupFiniteMap Hand1 (_cRelsBouts (_cRels contestant)) of
                Just (Present boutId) -> boutId
                _ -> error "No active bout found"

instance ValidatableIntent 'IPlayerSplit where
    validateIntent ctx = do
        handV <- derefV (pspICtxHandId ctx)
        contestantV <- derefV (pspICtxContestantId ctx)
        playerV <- derefV (pspICtxPlayerId ctx)
        return $ validatePlayerSplitData <$> handV <*> contestantV <*> playerV
      where
        validatePlayerSplitData hand contestant player =
            let handData = _hAttrsHand (_hAttrs hand)
                bankroll = _pAttrsBankroll (_pAttrs player)
                boutId = extractActiveBout contestant
                currentSplitCount = getCurrentSplitCountFromContestant contestant
             in (pspICtxContestantId ctx, pspICtxHandId ctx, boutId, pspICtxPlayerId ctx, handData, bankroll, currentSplitCount)

        extractActiveBout contestant =
            let activeHandIx = _cAttrsActiveHandIx (_cAttrs contestant)
             in case lookupFiniteMap activeHandIx (_cRelsBouts (_cRels contestant)) of
                    Just (Present boutId) -> boutId
                    _ -> error "No active bout found"
        getCurrentSplitCountFromContestant contestant =
            let bouts' = _cRelsBouts (_cRels contestant)
                occupiedCount = length [ix | (ix, Present _) <- toListFiniteMap bouts']
             in max 0 (occupiedCount - 1)
