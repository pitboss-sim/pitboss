{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Simulation.Intent.Generate where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Causality.Validate
import Pitboss.FSM
import Pitboss.Simulation.Agents.Player.Advantage
import Pitboss.Simulation.Agents.Player.Basic
import Pitboss.Simulation.Agents.Player.Perfect
import Pitboss.Simulation.Agents.Player.Superstitious
import Pitboss.Simulation.Agents.Types
import Pitboss.Simulation.Event
import System.Random (StdGen)

generateContestantIntent ::
    SomePlayerArchetype ->
    ContestantId ->
    StdGen ->
    Reader TickCacheContext (Either ValidationErrors BlackjackEvent)
generateContestantIntent archetype contestantId gen = runExceptT $ do
    contestant <- ExceptT $ validationToEither <$> derefV contestantId
    let activeHandIx = _cAttrsActiveHandIx (_cAttrs contestant)
        maybeBoutId = lookupFiniteMap activeHandIx (_cRelsBouts (_cRels contestant))
    boutId <- case maybeBoutId of
        Just (Present bid) -> pure bid
        _ -> throwError $ ValidationErrors [EntityNotFound "No active bout"]
    bout <- ExceptT $ validationToEither <$> derefV boutId
    let playerHandId' = _bRelsPlayerHand (_bRels bout)
    playerHand <- ExceptT $ validationToEither <$> derefV playerHandId'
    case _hModesHandFSM (_hModes playerHand) of
        ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM) -> generateEvent contestant bout playerHand
        ContestantHandFSM (SomeContestantHandFSM CHHittingFSM) -> generateEvent contestant bout playerHand
        _ -> throwError $ ValidationErrors [EntityNotFound "Hand not in decision state"]
  where
    generateEvent :: EntityState 'Contestant -> EntityState 'Bout -> EntityState 'Hand -> ExceptT ValidationErrors (Reader TickCacheContext) BlackjackEvent
    generateEvent contestant bout playerHand = do
        ctx <- ExceptT $ buildContestantContext contestant bout playerHand
        let move = evalState (decideByArchetype archetype ctx) gen
        pure $ moveToEvent contestantId (playerHandId bout) move

    playerHandId bout = _bRelsPlayerHand (_bRels bout)

validationToEither :: Validation e a -> Either e a
validationToEither (Success a) = Right a
validationToEither (Failure e) = Left e

buildContestantContext :: EntityState 'Contestant -> EntityState 'Bout -> EntityState 'Hand -> Reader TickCacheContext (Either ValidationErrors BoutContext)
buildContestantContext contestant bout playerHand = runExceptT $ do
    let dealerHandId = _bRelsDealerHand (_bRels bout)
        tableId = _bRelsTable (_bRels bout)

    (dealerHand, table) <- ExceptT $ validationToEither <$> validateContextEntities dealerHandId tableId

    let dealerCards = _hAttrsHand (_hAttrs dealerHand)
        offering = _tAttrsOffering (_tAttrs table)
        handIndex = _cAttrsActiveHandIx (_cAttrs contestant)
        currentSplitCount = countActiveSplits contestant

    upcard <- case dealerUpcard dealerCards of
        Just upcard -> pure upcard
        Nothing -> throwError $ ValidationErrors [EntityNotFound "No dealer upcard available"]

    pure
        BoutContext
            { _contextContestantHand = _hAttrsHand (_hAttrs playerHand)
            , _contextDealerUpcard = upcard
            , _contextOffering = offering
            , _contextCanDouble = handIndex == Hand1
            , _contextCanSplit = canSplitInContext (_hAttrsHand (_hAttrs playerHand)) offering currentSplitCount handIndex
            , _contextCanSurrender = handIndex == Hand1
            , _contextHandIx = handIndex
            }
  where
    validateContextEntities dealerHandId tableId = do
        dealerHandV <- derefV dealerHandId
        tableV <- derefV tableId
        pure $ (,) <$> dealerHandV <*> tableV

countActiveSplits :: EntityState 'Contestant -> Int
countActiveSplits contestant =
    let bouts' = _cRelsBouts (_cRels contestant)
        allHandIxs = [Hand1, Hand2, Hand3, Hand4]
        occupiedCount = length [ix | ix <- allHandIxs, case lookupFiniteMap ix bouts' of Just (Present _) -> True; _ -> False]
     in max 0 (occupiedCount - 1)

canSplitInContext :: SomeHand -> Offering -> Int -> HandIx -> Bool
canSplitInContext hand offering currentSplitCount handIx =
    let rules = gameRuleSet offering
        withinHandLimit = handIx < Hand4
     in isLegalSplit hand rules currentSplitCount && withinHandLimit

moveToEvent :: ContestantId -> HandId -> Move -> BlackjackEvent
moveToEvent contestantId handId = \case
    MStand -> ContestantStood contestantId handId
    MHit -> ContestantHit contestantId handId
    MDouble -> ContestantDoubledDown contestantId handId
    MSplit -> ContestantSplit contestantId handId
    MSurrender -> ContestantSurrendered contestantId handId

decideByArchetype :: SomePlayerArchetype -> BoutContext -> State StdGen Move
decideByArchetype archetype ctx = case archetype of
    SomePlayerBasicStrategy arch ->
        getBasicStrategyMove (bsConfig arch) ctx
    SomePlayerPerfect arch ->
        getPerfectMove (pfConfig arch) ctx
    SomePlayerAdvantage arch ->
        getAdvantageMove (advConfig arch) (advState arch) ctx
    SomePlayerSuperstitious arch ->
        getSuperstitiousMove (ssConfig arch) ctx

generateDealerIntent ::
    DealerId ->
    Reader TickCacheContext (Either ValidationErrors BlackjackEvent)
generateDealerIntent dealerId = runExceptT $ do
    dealer <- ExceptT $ validationToEither <$> derefV dealerId
    handId <- case _dRelsActiveHand (_dRels dealer) of
        Present hid -> pure hid
        Absent -> throwError $ ValidationErrors [EntityNotFound "Dealer has no active hand"]
    hand <- ExceptT $ validationToEither <$> derefV handId
    case _hRelsOwner (_hRels hand) of
        DealerOwner ownerId | ownerId == dealerId ->
            case _hModesHandFSM (_hModes hand) of
                DealerHandFSM (SomeDealerHandFSM DHDealingFSM) ->
                    case _dRelsActiveTable (_dRels dealer) of
                        Present tableId -> do
                            table <- ExceptT $ validationToEither <$> derefV tableId
                            let rules = gameRuleSet (_tAttrsOffering (_tAttrs table))
                                dealerHand = _hAttrsHand (_hAttrs hand)
                            if dealerShouldHit rules dealerHand
                                then pure $ DealerHit dealerId handId
                                else pure $ DealerStood dealerId handId
                        Absent -> throwError $ ValidationErrors [EntityNotFound "Dealer has no active table"]
                _ -> throwError $ ValidationErrors [EntityNotFound "Dealer hand not in dealing state"]
        _ -> throwError $ ValidationErrors [EntityNotFound "Hand not owned by dealer"]
