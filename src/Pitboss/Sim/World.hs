{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Sim.World where

import Control.Lens (Lens', lens)
import GHC.Generics (Generic)
import Pitboss.Sim.State.Actor
import Pitboss.Sim.State.Round
import Pitboss.Sim.State.Shoe
import Pitboss.Sim.State.Spot
import Pitboss.Sim.State.SpotHand
import Pitboss.Sim.State.Table
import Pitboss.Sim.World.Identifier
import Pitboss.Sim.World.Registry (Registry)

-- global simulation state

data WorldState = WorldState
  { _actors :: Registry ActorId ActorState,
    _tables :: Registry TableId SomeTableState,
    _shoes :: Registry ShoeId ShoeState,
    _rounds :: Registry RoundId RoundState,
    _spots :: Registry SpotId SpotState,
    _spotHands :: Registry SpotHandId SpotHandState
  }
  deriving stock (Generic, Show)

emptyWorldState :: WorldState
emptyWorldState =
  WorldState
    { _actors = mempty,
      _tables = mempty,
      _shoes = mempty,
      _rounds = mempty,
      _spots = mempty,
      _spotHands = mempty
    }

-- lenses

lensActors :: Lens' WorldState (Registry ActorId ActorState)
lensActors = lens _actors (\s x -> s {_actors = x})

lensTables :: Lens' WorldState (Registry TableId SomeTableState)
lensTables = lens _tables (\s x -> s {_tables = x})

lensShoes :: Lens' WorldState (Registry ShoeId ShoeState)
lensShoes = lens _shoes (\s x -> s {_shoes = x})

lensRounds :: Lens' WorldState (Registry RoundId RoundState)
lensRounds = lens _rounds (\s x -> s {_rounds = x})

lensSpots :: Lens' WorldState (Registry SpotId SpotState)
lensSpots = lens _spots (\s x -> s {_spots = x})

lensSpotHands :: Lens' WorldState (Registry SpotHandId SpotHandState)
lensSpotHands = lens _spotHands (\s x -> s {_spotHands = x})
