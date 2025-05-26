{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.State.Cache (
    EntityCache (..),
    CacheContext (..),
    Deref (..),
    playerSpotL,
    playerL,
    dealerL,
    mkCache,
    populateCache,
    withCache,
) where

import Control.Lens
import Control.Monad.Reader
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Word (Word64)

import Pitboss.State.Delta.Instances.Incremental
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Lenses
import Pitboss.State.Entity.Types
import Pitboss.State.Registry
import Pitboss.State.Timeline.Reconstruction
import Pitboss.State.Types.Core
import Prelude hiding (round)

data EntityCache = EntityCache
    { _cacheDealer :: IHM.InsOrdHashMap Word64 (EntityState 'Dealer)
    , _cachePlayer :: IHM.InsOrdHashMap Word64 (EntityState 'Player)
    , _cacheDealerHand :: IHM.InsOrdHashMap Word64 (EntityState 'DealerHand)
    , _cacheDealerRound :: IHM.InsOrdHashMap Word64 (EntityState 'DealerRound)
    , _cacheOffering :: IHM.InsOrdHashMap Word64 (EntityState 'Offering)
    , _cachePlayerHand :: IHM.InsOrdHashMap Word64 (EntityState 'PlayerHand)
    , _cachePlayerSpot :: IHM.InsOrdHashMap Word64 (EntityState 'PlayerSpot)
    , _cacheTable :: IHM.InsOrdHashMap Word64 (EntityState 'Table)
    , _cacheTableShoe :: IHM.InsOrdHashMap Word64 (EntityState 'TableShoe)
    , _cacheTick :: Tick
    }

makeLenses ''EntityCache

data CacheContext = CacheContext
    { _ctxCache :: EntityCache
    , _ctxTick :: Tick
    }

makeLenses ''CacheContext

class (MonadReader CacheContext m) => Deref id m where
    type DerefTarget id
    deref :: id -> m (Maybe (DerefTarget id))

instance (MonadReader CacheContext m) => Deref (EntityId 'Dealer) m where
    type DerefTarget (EntityId 'Dealer) = EntityState 'Dealer
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ IHM.lookup entropy (cache ^. cacheDealer)

instance (MonadReader CacheContext m) => Deref (EntityId 'Player) m where
    type DerefTarget (EntityId 'Player) = EntityState 'Player
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ IHM.lookup entropy (cache ^. cachePlayer)

instance (MonadReader CacheContext m) => Deref (EntityId 'PlayerSpot) m where
    type DerefTarget (EntityId 'PlayerSpot) = EntityState 'PlayerSpot
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ IHM.lookup entropy (cache ^. cachePlayerSpot)

instance (MonadReader CacheContext m) => Deref (EntityId 'PlayerHand) m where
    type DerefTarget (EntityId 'PlayerHand) = EntityState 'PlayerHand
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ IHM.lookup entropy (cache ^. cachePlayerHand)

instance (MonadReader CacheContext m) => Deref (EntityId 'DealerRound) m where
    type DerefTarget (EntityId 'DealerRound) = EntityState 'DealerRound
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ IHM.lookup entropy (cache ^. cacheDealerRound)

instance (MonadReader CacheContext m) => Deref (EntityId 'DealerHand) m where
    type DerefTarget (EntityId 'DealerHand) = EntityState 'DealerHand
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ IHM.lookup entropy (cache ^. cacheDealerHand)

instance (MonadReader CacheContext m) => Deref (EntityId 'Offering) m where
    type DerefTarget (EntityId 'Offering) = EntityState 'Offering
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ IHM.lookup entropy (cache ^. cacheOffering)

instance (MonadReader CacheContext m) => Deref (EntityId 'Table) m where
    type DerefTarget (EntityId 'Table) = EntityState 'Table
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ IHM.lookup entropy (cache ^. cacheTable)

instance (MonadReader CacheContext m) => Deref (EntityId 'TableShoe) m where
    type DerefTarget (EntityId 'TableShoe) = EntityState 'TableShoe
    deref (EntityId entropy) = do
        cache <- view ctxCache
        pure $ IHM.lookup entropy (cache ^. cacheTableShoe)

playerSpotL :: (MonadReader CacheContext m) => EntityState 'PlayerHand -> m (Maybe (EntityState 'PlayerSpot))
playerSpotL playerHand = do
    let spotId = playerHand ^. phRels . phRelsBelongsToPlayerSpot
    deref spotId

playerL :: (MonadReader CacheContext m) => EntityState 'PlayerSpot -> m (Maybe (EntityState 'Player))
playerL playerSpot = do
    let playerId = playerSpot ^. psRels . psEntityRelsPlayerId
    deref playerId

dealerL :: (MonadReader CacheContext m) => EntityState 'PlayerHand -> m (Maybe (EntityState 'Dealer))
dealerL playerHand = do
    maybeSpot <- playerSpotL playerHand
    case maybeSpot of
        Nothing -> pure Nothing
        Just spot -> do
            let roundId = spot ^. psRels . psEntityRelsRoundId
            maybeRound <- deref roundId
            case maybeRound of
                Nothing -> pure Nothing
                Just round -> do
                    cache <- view ctxCache
                    let dealerMap = cache ^. cacheDealer
                        dealerIds :: [EntityId 'Dealer] = do
                            (entropy, dealer) <- IHM.toList dealerMap
                            case dealer ^. dRels . dRelsActiveRound of
                                Just activeRound | activeRound == roundId -> [EntityId entropy]
                                _ -> []
                    case dealerIds of
                        (dealerId : _) -> deref dealerId
                        [] -> pure Nothing

mkCache :: Tick -> EntityCache
mkCache tick =
    EntityCache
        { _cacheDealer = IHM.empty
        , _cachePlayer = IHM.empty
        , _cacheDealerHand = IHM.empty
        , _cacheDealerRound = IHM.empty
        , _cacheOffering = IHM.empty
        , _cachePlayerHand = IHM.empty
        , _cachePlayerSpot = IHM.empty
        , _cacheTable = IHM.empty
        , _cacheTableShoe = IHM.empty
        , _cacheTick = tick
        }

populateCache ::
    Registry 'Player (SomeDelta 'Player) ->
    Registry 'PlayerHand (SomeDelta 'PlayerHand) ->
    Registry 'PlayerSpot (SomeDelta 'PlayerSpot) ->
    Registry 'Dealer (SomeDelta 'Dealer) ->
    Registry 'DealerHand (SomeDelta 'DealerHand) ->
    Registry 'DealerRound (SomeDelta 'DealerRound) ->
    Registry 'Offering (SomeDelta 'Offering) ->
    Registry 'Table (SomeDelta 'Table) ->
    Registry 'TableShoe (SomeDelta 'TableShoe) ->
    Tick ->
    EntityCache
populateCache playerReg handReg spotReg dealerReg dealerHandReg dealerRoundReg offeringReg tableReg tableShoeReg tick =
    let baseCache = mkCache tick

        populateFromRegistry ::
            (IncrementalWithWitness k) =>
            Registry k (SomeDelta k) ->
            (EntityCache -> IHM.InsOrdHashMap Word64 (EntityState k)) ->
            (EntityCache -> IHM.InsOrdHashMap Word64 (EntityState k) -> EntityCache) ->
            EntityCache ->
            EntityCache
        populateFromRegistry registry getter setter cache =
            let entities = do
                    (entropy, timeline) <- IHM.toList (unRegistry registry)
                    case reconstructAt timeline tick of
                        Just entityState -> [(entropy, entityState)]
                        Nothing -> []
             in setter cache (getter cache <> IHM.fromList entities)
     in baseCache
            & populateFromRegistry playerReg _cachePlayer (\c m -> c{_cachePlayer = m})
            & populateFromRegistry handReg _cachePlayerHand (\c m -> c{_cachePlayerHand = m})
            & populateFromRegistry spotReg _cachePlayerSpot (\c m -> c{_cachePlayerSpot = m})
            & populateFromRegistry dealerReg _cacheDealer (\c m -> c{_cacheDealer = m})
            & populateFromRegistry dealerHandReg _cacheDealerHand (\c m -> c{_cacheDealerHand = m})
            & populateFromRegistry dealerRoundReg _cacheDealerRound (\c m -> c{_cacheDealerRound = m})
            & populateFromRegistry offeringReg _cacheOffering (\c m -> c{_cacheOffering = m})
            & populateFromRegistry tableReg _cacheTable (\c m -> c{_cacheTable = m})
            & populateFromRegistry tableShoeReg _cacheTableShoe (\c m -> c{_cacheTableShoe = m})

withCache :: EntityCache -> Reader CacheContext a -> a
withCache cache computation = runReader computation (CacheContext cache (cache ^. cacheTick))
