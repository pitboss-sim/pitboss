{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Pitboss.State.TickCache (
    TickCache (..),
    TickCacheContext (..),
    Deref (..),
    mkTickCache,
    populateTickCache,
    withTickCache,
) where

import Control.Lens
import Control.Monad.Reader
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Word (Word64)

import Pitboss.State.Delta.Instances.Incremental
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Registry
import Pitboss.State.Timeline.Reconstruction
import Pitboss.State.Types.Core
import Prelude hiding (round)

data TickCache = TickCache
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

makeLenses ''TickCache

data TickCacheContext = TickCacheContext
    { _ctxTickCache :: TickCache
    , _ctxTick :: Tick
    }

makeLenses ''TickCacheContext

class (MonadReader TickCacheContext m) => Deref id m where
    type DerefTarget id
    deref :: id -> m (Maybe (DerefTarget id))

instance (MonadReader TickCacheContext m) => Deref (EntityId 'Dealer) m where
    type DerefTarget (EntityId 'Dealer) = EntityState 'Dealer
    deref (EntityId entropy) = do
        cache <- view ctxTickCache
        pure $ IHM.lookup entropy (cache ^. cacheDealer)

instance (MonadReader TickCacheContext m) => Deref (EntityId 'Player) m where
    type DerefTarget (EntityId 'Player) = EntityState 'Player
    deref (EntityId entropy) = do
        cache <- view ctxTickCache
        pure $ IHM.lookup entropy (cache ^. cachePlayer)

instance (MonadReader TickCacheContext m) => Deref (EntityId 'PlayerSpot) m where
    type DerefTarget (EntityId 'PlayerSpot) = EntityState 'PlayerSpot
    deref (EntityId entropy) = do
        cache <- view ctxTickCache
        pure $ IHM.lookup entropy (cache ^. cachePlayerSpot)

instance (MonadReader TickCacheContext m) => Deref (EntityId 'PlayerHand) m where
    type DerefTarget (EntityId 'PlayerHand) = EntityState 'PlayerHand
    deref (EntityId entropy) = do
        cache <- view ctxTickCache
        pure $ IHM.lookup entropy (cache ^. cachePlayerHand)

instance (MonadReader TickCacheContext m) => Deref (EntityId 'DealerRound) m where
    type DerefTarget (EntityId 'DealerRound) = EntityState 'DealerRound
    deref (EntityId entropy) = do
        cache <- view ctxTickCache
        pure $ IHM.lookup entropy (cache ^. cacheDealerRound)

instance (MonadReader TickCacheContext m) => Deref (EntityId 'DealerHand) m where
    type DerefTarget (EntityId 'DealerHand) = EntityState 'DealerHand
    deref (EntityId entropy) = do
        cache <- view ctxTickCache
        pure $ IHM.lookup entropy (cache ^. cacheDealerHand)

instance (MonadReader TickCacheContext m) => Deref (EntityId 'Offering) m where
    type DerefTarget (EntityId 'Offering) = EntityState 'Offering
    deref (EntityId entropy) = do
        cache <- view ctxTickCache
        pure $ IHM.lookup entropy (cache ^. cacheOffering)

instance (MonadReader TickCacheContext m) => Deref (EntityId 'Table) m where
    type DerefTarget (EntityId 'Table) = EntityState 'Table
    deref (EntityId entropy) = do
        cache <- view ctxTickCache
        pure $ IHM.lookup entropy (cache ^. cacheTable)

instance (MonadReader TickCacheContext m) => Deref (EntityId 'TableShoe) m where
    type DerefTarget (EntityId 'TableShoe) = EntityState 'TableShoe
    deref (EntityId entropy) = do
        cache <- view ctxTickCache
        pure $ IHM.lookup entropy (cache ^. cacheTableShoe)

mkTickCache :: Tick -> TickCache
mkTickCache tick =
    TickCache
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

populateTickCache ::
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
    TickCache
populateTickCache playerReg handReg spotReg dealerReg dealerHandReg dealerRoundReg offeringReg tableReg tableShoeReg tick =
    let baseTickCache = mkTickCache tick

        populateFromRegistry ::
            (IncrementalWithWitness k) =>
            Registry k (SomeDelta k) ->
            (TickCache -> IHM.InsOrdHashMap Word64 (EntityState k)) ->
            (TickCache -> IHM.InsOrdHashMap Word64 (EntityState k) -> TickCache) ->
            TickCache ->
            TickCache
        populateFromRegistry registry getter setter cache =
            let entities = do
                    (entropy, timeline) <- IHM.toList (unRegistry registry)
                    case reconstructAt timeline tick of
                        Just entityState -> [(entropy, entityState)]
                        Nothing -> []
             in setter cache (getter cache <> IHM.fromList entities)
     in baseTickCache
            & populateFromRegistry playerReg _cachePlayer (\c m -> c{_cachePlayer = m})
            & populateFromRegistry handReg _cachePlayerHand (\c m -> c{_cachePlayerHand = m})
            & populateFromRegistry spotReg _cachePlayerSpot (\c m -> c{_cachePlayerSpot = m})
            & populateFromRegistry dealerReg _cacheDealer (\c m -> c{_cacheDealer = m})
            & populateFromRegistry dealerHandReg _cacheDealerHand (\c m -> c{_cacheDealerHand = m})
            & populateFromRegistry dealerRoundReg _cacheDealerRound (\c m -> c{_cacheDealerRound = m})
            & populateFromRegistry offeringReg _cacheOffering (\c m -> c{_cacheOffering = m})
            & populateFromRegistry tableReg _cacheTable (\c m -> c{_cacheTable = m})
            & populateFromRegistry tableShoeReg _cacheTableShoe (\c m -> c{_cacheTableShoe = m})

withTickCache :: TickCache -> Reader TickCacheContext a -> a
withTickCache cache computation = runReader computation (TickCacheContext cache (cache ^. cacheTick))
