{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Intent.Context.PlayerPlaying where

import Control.Monad.Reader (Reader)
import Pitboss.Blackjack.Types.Core
import Pitboss.Causality.TickCache
import Pitboss.Causality.Types.Core
import Pitboss.Simulation.Intent.Context.Types.Core
import Pitboss.Simulation.Intent.Types

buildPlayingPlayerSpine :: PlayerId -> BoutId -> Reader TickCacheContext (Maybe (Spine 'PlayingPlayer))
buildPlayingPlayerSpine playerId boutId = do
    mBout <- derefV boutId
    case mBout of
        Nothing -> pure Nothing
        Just bout -> do
            let tableId = undefined
            let roundId = undefined
            let dealerId = undefined
            mTable <- derefV tableId
            mRound <- derefV roundId
            mPlayer <- derefV playerId
            mDealer <- derefV dealerId
            pure $ PlayingPlayerSpine <$> mTable <*> mRound <*> pure bout <*> mPlayer <*> mDealer

buildPlayingPlayerContext :: PlayerId -> BoutId -> Reader TickCacheContext (Maybe (IntentCtx 'PlayingPlayer))
buildPlayingPlayerContext playerId boutId = do
    mSpine <- buildPlayingPlayerSpine playerId boutId
    case mSpine of
        Nothing -> pure Nothing
        Just spine -> do
            let availableMoves = computeAvailableMoves spine
            let currentBet = computeCurrentBet spine
            pure $ Just $ PlayingPlayerCtx spine availableMoves currentBet

computeAvailableMoves :: Spine 'PlayingPlayer -> [Move]
computeAvailableMoves = undefined

computeCurrentBet :: Spine 'PlayingPlayer -> Chips
computeCurrentBet = undefined
