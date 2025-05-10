{-# LANGUAGE DerivingStrategies #-}

module Pitboss.World where

import Control.Lens (Lens', lens)
import GHC.Generics (Generic)
import Pitboss.World.State.Actor
import Pitboss.World.State.Hand (HandState)
import Pitboss.World.State.Round
import Pitboss.World.State.Shoe
import Pitboss.World.State.Spot
import Pitboss.World.Types.Identifier
import Pitboss.World.Types.Registry (Registry)

-- global simulation state

data WorldState = WorldState
  { _actors :: Registry ActorId ActorState,
    _shoes :: Registry ShoeId ShoeState,
    _rounds :: Registry RoundId RoundState,
    _spots :: Registry SpotId SpotState,
    _spotHands :: Registry HandId HandState
  }
  deriving stock (Generic, Show)

emptyWorldState :: WorldState
emptyWorldState =
  WorldState
    { _actors = mempty,
      _shoes = mempty,
      _rounds = mempty,
      _spots = mempty,
      _spotHands = mempty
    }

-- lenses

lensActors :: Lens' WorldState (Registry ActorId ActorState)
lensActors = lens _actors (\s x -> s {_actors = x})

lensShoes :: Lens' WorldState (Registry ShoeId ShoeState)
lensShoes = lens _shoes (\s x -> s {_shoes = x})

lensRounds :: Lens' WorldState (Registry RoundId RoundState)
lensRounds = lens _rounds (\s x -> s {_rounds = x})

lensSpots :: Lens' WorldState (Registry SpotId SpotState)
lensSpots = lens _spots (\s x -> s {_spots = x})

lensHands :: Lens' WorldState (Registry HandId HandState)
lensHands = lens _spotHands (\s x -> s {_spotHands = x})
