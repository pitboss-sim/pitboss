{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Simulation.Intent.Generate where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation.Agents.Player.Advantage
import Pitboss.Simulation.Agents.Player.Basic
import Pitboss.Simulation.Agents.Player.Perfect
import Pitboss.Simulation.Agents.Player.Superstitious
import Pitboss.Simulation.Agents.Types
import Pitboss.Simulation.Event
import Pitboss.Simulation.Event.Generate
import Pitboss.Simulation.Intent.Types
import System.Random (StdGen)

generateBoutIntent ::
    SomePlayerArchetype ->
    BoutId ->
    StdGen ->
    Reader TickCacheContext (Either ValidationErrors SimulationEvent)
generateBoutIntent archetype boutPlayerId gen = runExceptT $ do
    boutPlayer <- ExceptT $ validationToEither <$> derefV boutPlayerId
    let activeHandIx = _bAttrsActiveHandIx (_bAttrs boutPlayer)
        maybeBoutId = lookupFiniteMap activeHandIx (_bRelsPlayerBouts (_bRels boutPlayer))
    boutId <- case maybeBoutId of
        Just (Present bid) -> pure bid
        _ -> throwError $ ValidationErrors [EntityNotFound "No active bout"]
    bout <- ExceptT $ validationToEither <$> derefV boutId
    case _bModesPlayerHandFSM (_bModes boutPlayer) of
        SomePlayerHandFSM PHDecisionFSM -> generateEvent boutPlayer bout
        SomePlayerHandFSM PHHittingFSM -> generateEvent boutPlayer bout
        _ -> throwError $ ValidationErrors [EntityNotFound "Hand not in decision state"]
  where
    generateEvent :: EntityState 'Bout -> EntityState 'Bout -> ExceptT ValidationErrors (Reader TickCacheContext) SimulationEvent
    generateEvent boutPlayer bout = do
        ctx <- ExceptT $ buildBoutContext boutPlayer bout
        let move = evalState (decideByArchetype archetype ctx) gen
        let playerId = _bRelsPlayer (_bRels boutPlayer)
            tableId = _bRelsTable (_bRels bout)
            intent = boutPlayerMoveToIntent boutPlayerId playerId tableId move
        pure $ intentToEvent intent

validationToEither :: Validation e a -> Either e a
validationToEither (Success a) = Right a
validationToEither (Failure e) = Left e

buildBoutContext :: EntityState 'Bout -> EntityState 'Bout -> Reader TickCacheContext (Either ValidationErrors BoutContext)
buildBoutContext boutPlayer bout = runExceptT $ do
    let tableId = _bRelsTable (_bRels bout)
        _roundId = _bRelsRound (_bRels bout)

    table <- ExceptT $ validationToEither <$> derefV tableId

    let offering = _tAttrsOffering (_tAttrs table)
        handIndex = _bAttrsActiveHandIx (_bAttrs boutPlayer)
        currentSplitCount = countActiveSplits boutPlayer
        playerHand = _bAttrsPlayerHand (_bAttrs boutPlayer)

    let dealerHand = _bAttrsDealerHand (_bAttrs bout)

    upcard <- case dealerUpcard dealerHand of
        Just upcard -> pure upcard
        Nothing -> throwError $ ValidationErrors [EntityNotFound "No dealer upcard available"]

    pure
        BoutContext
            { _contextBoutPlayerHand = playerHand
            , _contextDealerUpcard = upcard
            , _contextOffering = offering
            , _contextCanDouble = handIndex == Hand1
            , _contextCanSplit = canSplitInContext playerHand offering currentSplitCount handIndex
            , _contextCanSurrender = handIndex == Hand1
            , _contextHandIx = handIndex
            }

countActiveSplits :: EntityState 'Bout -> Int
countActiveSplits boutPlayer =
    let bouts' = _bRelsPlayerBouts (_bRels boutPlayer)
        allHandIxs = [Hand1, Hand2, Hand3, Hand4]
        occupiedCount = length [ix | ix <- allHandIxs, case lookupFiniteMap ix bouts' of Just (Present _) -> True; _ -> False]
     in max 0 (occupiedCount - 1)

canSplitInContext :: SomeHand -> Offering -> Int -> HandIx -> Bool
canSplitInContext hand offering currentSplitCount handIx =
    let rules = gameRuleSet offering
        withinHandLimit = handIx < Hand4
     in isLegalSplit hand rules currentSplitCount && withinHandLimit

boutPlayerMoveToIntent :: BoutId -> PlayerId -> TableId -> Move -> SomeIntent
boutPlayerMoveToIntent boutPlayerId playerId tableId = \case
    MStand -> BoutPlayerStandIntent (BoutPlayerStandCtx boutPlayerId)
    MHit -> BoutPlayerHitIntent (BoutPlayerHitCtx boutPlayerId)
    MDouble -> BoutPlayerDoubleIntent (BoutPlayerDoubleCtx boutPlayerId playerId tableId)
    MSplit -> BoutPlayerSplitIntent (BoutPlayerSplitCtx boutPlayerId playerId tableId)
    MSurrender -> BoutPlayerSurrenderIntent (BoutPlayerSurrenderCtx boutPlayerId tableId)

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

generateDealerIntent :: DealerId -> Reader TickCacheContext (Either ValidationErrors BlackjackEvent)
generateDealerIntent _dealerId = pure $ Left $ ValidationErrors [EntityNotFound "Dealer intent generation needs implementation with embedded hands"]

findBoutByRound :: RoundId -> IHM.InsOrdHashMap BoutId (EntityState 'Bout) -> Maybe BoutId
findBoutByRound roundId = IHM.foldlWithKey' findByRound Nothing
  where
    findByRound :: Maybe BoutId -> BoutId -> EntityState 'Bout -> Maybe BoutId
    findByRound acc bdId (EBout _ _ rels)
        | _bRelsRound rels == roundId = Just bdId
        | otherwise = acc
