{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Intent.Context.PlayerBetting where

import Control.Monad.Reader (Reader)
import Pitboss.Blackjack.Types.Core
import Pitboss.Causality.TickCache
import Pitboss.Causality.Types.Core
import Pitboss.Simulation.Intent.Context.Types.Core
import Pitboss.Simulation.Intent.Types

buildBettingPlayerSpine :: PlayerId -> TableId -> Reader TickCacheContext (Maybe (Spine 'BettingPlayer))
buildBettingPlayerSpine playerId tableId = do
    mTable <- deref tableId
    mPlayer <- deref playerId
    pure $ BettingPlayerSpine <$> mTable <*> mPlayer

buildBettingPlayerContext :: PlayerId -> TableId -> Reader TickCacheContext (Maybe (IntentCtx 'BettingPlayer))
buildBettingPlayerContext playerId tableId = do
    mSpine <- buildBettingPlayerSpine playerId tableId
    case mSpine of
        Nothing -> pure Nothing
        Just spine -> do
            let availableFunds = computeAvailableFunds spine
            let tableLimits = computeTableLimits spine
            pure $ Just $ BettingPlayerCtx spine availableFunds tableLimits

computeAvailableFunds :: Spine 'BettingPlayer -> Chips
computeAvailableFunds = undefined

computeTableLimits :: Spine 'BettingPlayer -> (Chips, Chips)
computeTableLimits = undefined
