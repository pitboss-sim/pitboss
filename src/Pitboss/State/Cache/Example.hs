{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.State.Cache.Example where

import Control.Lens
import Control.Monad (join)
import Control.Monad.Reader
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Word (Word64)

import Pitboss.State.Cache
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Lenses
import Pitboss.State.Entity.Types
import Pitboss.State.Registry
import Pitboss.State.Types.Core
import Prelude hiding (round)

-- | Example of how to retrieve a player name from a hand through entity relationships
getPlayerNameFromHand :: EntityState 'PlayerHand -> Reader CacheContext (Maybe String)
getPlayerNameFromHand playerHand = do
    let spotId = playerHand ^. phRels . phRelsBelongsToPlayerSpot
        playerId spot = spot ^. psRels . psEntityRelsPlayerId

    maybeSpot <- deref spotId :: Reader CacheContext (Maybe (EntityState 'PlayerSpot))
    maybePlayer <- traverse (\spot -> deref (playerId spot) :: Reader CacheContext (Maybe (EntityState 'Player))) maybeSpot
    pure $ join maybePlayer ^? _Just . pAttrs . pAttrsName

-- | Example of traversing relationships to get player name from hand using case statements
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

{- | Example of traversing relationships to get player name from hand using lens combinators
This demonstrates a more concise approach compared to the case-based version above
-}
exampleTraversalLens :: EntityState 'PlayerHand -> Reader CacheContext (Maybe String)
exampleTraversalLens playerHand = do
    maybeSpot <- playerSpotL playerHand
    traverse
        ( \spot -> do
            maybePlayer <- playerL spot
            pure $ maybePlayer ^? _Just . pAttrs . pAttrsName
        )
        maybeSpot
        <&> join

{-
Usage examples:

Creating and using a cache:

```haskell
-- Create a cache from registries at a specific tick
let cache = populateCache
    playerRegistry
    playerHandRegistry
    playerSpotRegistry
    dealerRegistry
    dealerHandRegistry
    dealerRoundRegistry
    offeringRegistry
    tableRegistry
    tableShoeRegistry
    currentTick

-- Run a query using the cache
let result = withCache cache $ exampleTraversal somePlayerHand
```

Entity relationship traversal:
The Cache module provides functions for traversing entity relationships:
- playerSpotL: from PlayerHand to PlayerSpot
- playerL: from PlayerSpot to Player
- dealerL: from PlayerHand to Dealer (via PlayerSpot → DealerRound → Dealer)

These functions operate within a Reader CacheContext monad, allowing access
to the entity cache when resolving relationships.

Entity lifecycle:
Entities are reconstructed at a specific tick from registries which contain
deltas. The cache provides a snapshot of all entities at that tick for
efficient querying and relationship traversal.
-}
