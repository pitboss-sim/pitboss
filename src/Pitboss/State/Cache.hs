{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Pitboss.State.Cache (
    EntityCache (..),
    CacheContext (..),
    Deref (..),
    playerSpotL,
    playerL,
    dealerL,
    mkCache,
    withCache,
) where

import Control.Lens
import Control.Monad.Reader
import Data.HashMap.Strict qualified as HM
import Data.Word (Word64)

import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Registry
import Pitboss.State.Types.Core
import Pitboss.State.Entity.Lenses
import Prelude hiding (round)
import Control.Monad (join)

-- Entity cache - materialized view at a specific tick
data EntityCache = EntityCache
    { _cacheDealer :: HM.HashMap Word64 (EntityState 'Dealer)
    , _cachePlayer :: HM.HashMap Word64 (EntityState 'Player)
    , _cacheDealerHand :: HM.HashMap Word64 (EntityState 'DealerHand)
    , _cacheDealerRound :: HM.HashMap Word64 (EntityState 'DealerRound)
    , _cacheOffering :: HM.HashMap Word64 (EntityState 'Offering)
    , _cachePlayerHand :: HM.HashMap Word64 (EntityState 'PlayerHand)
    , _cachePlayerSpot :: HM.HashMap Word64 (EntityState 'PlayerSpot)
    , _cacheTable :: HM.HashMap Word64 (EntityState 'Table)
    , _cacheTableShoe :: HM.HashMap Word64 (EntityState 'TableShoe)
    , _cacheTick :: Tick
    }

makeLenses ''EntityCache

-- Context for lens operations - provides cache and current tick
data CacheContext = CacheContext
    { _ctxCache :: EntityCache
    , _ctxTick :: Tick
    }

makeLenses ''CacheContext

-- Deref typeclass - resolve EntityId to EntityState within cache context
class MonadReader CacheContext m => Deref id m where
    type DerefTarget id
    deref :: id -> m (Maybe (DerefTarget id))

-- Deref instances for each entity type
instance MonadReader CacheContext m => Deref (EntityId 'Dealer) m where
    type DerefTarget (EntityId 'Dealer) = EntityState 'Dealer
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ HM.lookup entropy (cache ^. cacheDealer)

instance MonadReader CacheContext m => Deref (EntityId 'Player) m where
    type DerefTarget (EntityId 'Player) = EntityState 'Player
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ HM.lookup entropy (cache ^. cachePlayer)

instance MonadReader CacheContext m => Deref (EntityId 'PlayerSpot) m where
    type DerefTarget (EntityId 'PlayerSpot) = EntityState 'PlayerSpot
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ HM.lookup entropy (cache ^. cachePlayerSpot)

instance MonadReader CacheContext m => Deref (EntityId 'PlayerHand) m where
    type DerefTarget (EntityId 'PlayerHand) = EntityState 'PlayerHand
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ HM.lookup entropy (cache ^. cachePlayerHand)

-- ... similar instances for other entity types

-- Lens-based traversals that automatically deref relationships
playerSpotL :: (MonadReader CacheContext m) => EntityState 'PlayerHand -> m (Maybe (EntityState 'PlayerSpot))
playerSpotL playerHand = do
    let spotId = playerHand ^. phRels . phRelsBelongsToPlayerSpot
    deref spotId

playerL :: (MonadReader CacheContext m) => EntityState 'PlayerSpot -> m (Maybe (EntityState 'Player))
playerL playerSpot = do
    let playerId = playerSpot ^. psRels . psEntityRelsPlayerId
    deref playerId

dealerL :: (MonadReader CacheContext m, Deref (EntityId DealerRound) m) => EntityState 'PlayerHand -> m (Maybe (EntityState 'Dealer))
dealerL playerHand = do
    maybeSpot <- playerSpotL playerHand
    case maybeSpot of
        Nothing -> pure Nothing
        Just spot -> do
            let roundId = spot ^. psRels . psEntityRelsRoundId
            maybeRound <- deref roundId
            case maybeRound of
                Nothing -> pure Nothing
                Just _ -> do
                    -- Navigate from round to dealer (would need this relationship)
                    pure Nothing -- Placeholder

getPlayerNameFromHand' :: EntityState 'PlayerHand -> Reader CacheContext (Maybe String)
getPlayerNameFromHand' playerHand = do
    let spotId = playerHand ^. phRels . phRelsBelongsToPlayerSpot
        playerId spot = spot ^. psRels . psEntityRelsPlayerId

    maybeSpot <- deref spotId :: Reader CacheContext (Maybe (EntityState 'PlayerSpot))
    maybePlayer <- traverse (\spot -> deref (playerId spot) :: Reader CacheContext (Maybe (EntityState 'Player))) maybeSpot
    pure $ join maybePlayer ^? _Just . pAttrs . pAttrsName

-- Cache construction from registries
mkCache ::
    Tick ->
    EntityCache
mkCache tick = EntityCache
    { _cacheDealer = HM.empty
    , _cachePlayer = HM.empty
    , _cacheDealerHand = HM.empty
    , _cacheDealerRound = HM.empty
    , _cacheOffering = HM.empty
    , _cachePlayerHand = HM.empty
    , _cachePlayerSpot = HM.empty
    , _cacheTable = HM.empty
    , _cacheTableShoe = HM.empty
    , _cacheTick = tick
    }

-- Helper to populate cache from registries
populateCache ::
    Registry 'Player (SomeDelta 'Player) ->
    Registry 'PlayerHand (SomeDelta 'PlayerHand) ->
    Registry 'PlayerSpot (SomeDelta 'PlayerSpot) ->
    -- ... other registries
    Tick ->
    EntityCache
populateCache playerReg handReg spotReg tick =
    let baseCache = mkCache tick
        -- Would populate each cache from registries using lookupEntityAtTick
    in baseCache -- Placeholder implementation

-- Utility to run cache-aware computations
withCache :: EntityCache -> Reader CacheContext a -> a
withCache cache computation = runReader computation (CacheContext cache (cache ^. cacheTick))

-- Example usage:
exampleTraversal :: EntityState 'PlayerHand -> Reader CacheContext (Maybe String)
exampleTraversal playerHand = do
    maybeSpot <- playerSpotL playerHand
    case maybeSpot of
        Nothing -> pure Nothing
        Just spot -> do
            maybePlayer <- playerL spot
            case maybePlayer of
                Nothing -> pure Nothing
                Just player -> pure $ Just (player ^. pAttrs . pAttrsName)

-- Even cleaner with lens combinators:
exampleTraversalLens :: EntityState 'PlayerHand -> Reader CacheContext (Maybe String)
exampleTraversalLens playerHand = do
    maybeSpot <- playerSpotL playerHand
    traverse (\spot -> do
        maybePlayer <- playerL spot
        pure $ maybePlayer ^? _Just . pAttrs . pAttrsName) maybeSpot <&> join

-- Usage in practice:
-- let cache = populateCache playerReg handReg spotReg currentTick
-- let result = withCache cache $ exampleTraversal somePlayerHand
