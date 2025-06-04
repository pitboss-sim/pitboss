{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Intent.Context.DealerPlaying where

import Control.Monad.Reader (Reader)
import Pitboss.Causality.TickCache
import Pitboss.Causality.Types.Core
import Pitboss.Simulation.Intent.Context.Types.Core
import Pitboss.Simulation.Intent.Types

buildPlayingDealerSpine :: DealerId -> BoutId -> Reader TickCacheContext (Maybe (Spine 'PlayingDealer))
buildPlayingDealerSpine dealerId boutId = do
    mBout <- derefV boutId
    case mBout of
        Nothing -> pure Nothing
        Just bout -> do
            let tableId = undefined
            let roundId = undefined
            let playerId = undefined
            mTable <- derefV tableId
            mRound <- derefV roundId
            mPlayer <- derefV playerId
            mDealer <- derefV dealerId
            pure $ PlayingDealerSpine <$> mTable <*> mRound <*> pure bout <*> mPlayer <*> mDealer

buildPlayingDealerContext :: DealerId -> BoutId -> Reader TickCacheContext (Maybe (IntentCtx 'PlayingDealer))
buildPlayingDealerContext dealerId boutId = do
    mSpine <- buildPlayingDealerSpine dealerId boutId
    case mSpine of
        Nothing -> pure Nothing
        Just spine -> do
            let availableMoves = computeDealerMoves spine
            pure $ Just $ PlayingDealerCtx spine availableMoves

computeDealerMoves :: Spine 'PlayingDealer -> [Move]
computeDealerMoves = undefined
