{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Pitboss.Causality.TickCache where

import Control.Lens
import Control.Monad.Reader
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Hashable (Hashable)

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Causality.Delta.IncrementalPart
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Registry
import Pitboss.Causality.Timeline.Reconstruction
import Pitboss.Causality.Types.Core
import Prelude hiding (round)

data TickCache = TickCache
    { _cacheBout :: IHM.InsOrdHashMap BoutId (EntityState 'Bout)
    , _cacheDealer :: IHM.InsOrdHashMap DealerId (EntityState 'Dealer)
    , _cachePlayer :: IHM.InsOrdHashMap PlayerId (EntityState 'Player)
    , _cacheRound :: IHM.InsOrdHashMap RoundId (EntityState 'Round)
    , _cacheShoe :: IHM.InsOrdHashMap ShoeId (EntityState 'Shoe)
    , _cacheTable :: IHM.InsOrdHashMap TableId (EntityState 'Table)
    , _cacheTick :: Tick
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeLenses ''TickCache

data TickCacheContext = TickCacheContext
    { _ctxTickCache :: TickCache
    , _ctxTick :: Tick
    }

makeLenses ''TickCacheContext

class (MonadReader TickCacheContext m) => Deref id m where
    type DerefTarget id
    deref :: id -> m (Maybe (DerefTarget id))

derefHelper ::
    (MonadReader TickCacheContext m, Hashable id) =>
    Getting (IHM.InsOrdHashMap id (EntityState k)) TickCache (IHM.InsOrdHashMap id (EntityState k)) ->
    id ->
    m (Maybe (EntityState k))
derefHelper cacheLens entityId = do
    cache <- view ctxTickCache
    pure $ IHM.lookup entityId (cache ^. cacheLens)

instance (MonadReader TickCacheContext m) => Deref BoutId m where
    type DerefTarget BoutId = EntityState 'Bout
    deref = derefHelper cacheBout

instance (MonadReader TickCacheContext m) => Deref PlayerId m where
    type DerefTarget PlayerId = EntityState 'Player
    deref = derefHelper cachePlayer

instance (MonadReader TickCacheContext m) => Deref DealerId m where
    type DerefTarget DealerId = EntityState 'Dealer
    deref = derefHelper cacheDealer

instance (MonadReader TickCacheContext m) => Deref RoundId m where
    type DerefTarget RoundId = EntityState 'Round
    deref = derefHelper cacheRound

instance (MonadReader TickCacheContext m) => Deref TableId m where
    type DerefTarget TableId = EntityState 'Table
    deref = derefHelper cacheTable

instance (MonadReader TickCacheContext m) => Deref ShoeId m where
    type DerefTarget ShoeId = EntityState 'Shoe
    deref = derefHelper cacheShoe

mkTickCache :: Tick -> TickCache
mkTickCache tick =
    TickCache
        { _cacheBout = IHM.empty
        , _cacheDealer = IHM.empty
        , _cachePlayer = IHM.empty
        , _cacheRound = IHM.empty
        , _cacheShoe = IHM.empty
        , _cacheTable = IHM.empty
        , _cacheTick = tick
        }

populateTickCache ::
    Registry 'Bout (SomeDelta 'Bout) ->
    Registry 'Dealer (SomeDelta 'Dealer) ->
    Registry 'Player (SomeDelta 'Player) ->
    Registry 'Round (SomeDelta 'Round) ->
    Registry 'Shoe (SomeDelta 'Shoe) ->
    Registry 'Table (SomeDelta 'Table) ->
    Tick ->
    TickCache
populateTickCache boutReg dealerReg playerReg roundReg shoeReg tableReg tick =
    let baseTickCache = mkTickCache tick
        populateFromRegistry ::
            (IncrementalPart k, Hashable (EntityIdFor k)) =>
            Registry k (SomeDelta k) ->
            (TickCache -> IHM.InsOrdHashMap (EntityIdFor k) (EntityState k)) ->
            (TickCache -> IHM.InsOrdHashMap (EntityIdFor k) (EntityState k) -> TickCache) ->
            TickCache ->
            TickCache
        populateFromRegistry registry getter setter cache =
            let entities = do
                    (entityId, timeline) <- IHM.toList (unRegistry registry)
                    case reconstructAt timeline tick of
                        Right entityState -> [(entityId, entityState)]
                        Left _err -> []
             in setter cache (getter cache <> IHM.fromList entities)
     in baseTickCache
            & populateFromRegistry boutReg _cacheBout (\c m -> c{_cacheBout = m})
            & populateFromRegistry dealerReg _cacheDealer (\c m -> c{_cacheDealer = m})
            & populateFromRegistry playerReg _cachePlayer (\c m -> c{_cachePlayer = m})
            & populateFromRegistry roundReg _cacheRound (\c m -> c{_cacheRound = m})
            & populateFromRegistry shoeReg _cacheShoe (\c m -> c{_cacheShoe = m})
            & populateFromRegistry tableReg _cacheTable (\c m -> c{_cacheTable = m})

withTickCache :: TickCache -> Reader TickCacheContext a -> a
withTickCache cache computation = runReader computation (TickCacheContext cache (cache ^. cacheTick))
