{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pitboss.Sim.World.Lens where

import Control.Lens
import Pitboss.Sim.State.Actor (ActorState)
import Pitboss.Sim.State.Round (RoundState)
import Pitboss.Sim.State.Shoe (ShoeState)
import Pitboss.Sim.State.Spot (SpotState)
import Pitboss.Sim.State.SpotHand (SpotHandState)
import Pitboss.Sim.State.Table (SomeTableState)
import Pitboss.Sim.World
import Pitboss.Sim.World.Identifier
import Pitboss.Sim.World.Registry

-- | Provide lenses for WorldState registry fields
class HasWorldState s where
  lensActors :: Lens' s (Registry ActorId ActorState)
  lensTables :: Lens' s (Registry TableId SomeTableState)
  lensShoes :: Lens' s (Registry ShoeId ShoeState)
  lensRounds :: Lens' s (Registry RoundId RoundState)
  lensSpots :: Lens' s (Registry SpotId SpotState)
  lensSpotHands :: Lens' s (Registry SpotHandId SpotHandState)

instance HasWorldState WorldState where
  lensActors = lens _actors (\s x -> s {_actors = x})
  lensTables = lens _tables (\s x -> s {_tables = x})
  lensShoes = lens _shoes (\s x -> s {_shoes = x})
  lensRounds = lens _rounds (\s x -> s {_rounds = x})
  lensSpots = lens _spots (\s x -> s {_spots = x})
  lensSpotHands = lens _spotHands (\s x -> s {_spotHands = x})

-- | Lens into a specific ID in its registry (polymorphic over entity kind)
atEntity :: (ToWord64 k, HasUid k, HasWorldState s) => Lens' s (Registry k v) -> k -> Lens' s (Maybe v)
atEntity registryLens k = registryLens . at (toWord64 k)

-- | Traversal for a present entity (like ix) — fails if the key is not present
ixEntity :: (ToWord64 k, HasUid k, HasWorldState s) => Lens' s (Registry k v) -> k -> Traversal' s v
ixEntity registryLens k = registryLens . ix (toWord64 k)
