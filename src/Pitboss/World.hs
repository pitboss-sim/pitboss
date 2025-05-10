{-# LANGUAGE DerivingStrategies #-}

module Pitboss.World where

import Control.Lens (Lens', lens)
import GHC.Generics (Generic)
import Pitboss.World.State.Actor
import Pitboss.World.State.Hand
import Pitboss.World.State.Offering (OfferingState)
import Pitboss.World.State.Round
import Pitboss.World.State.Shoe
import Pitboss.World.State.Spot
import Pitboss.World.Types.Identifier
import Pitboss.World.Types.Registry

-- global simulation state

data WorldState = WorldState
  { _offeringsReg :: Registry OfferingId OfferingState,
    _actorsReg :: Registry ActorId ActorState,
    _shoesReg :: Registry ShoeId ShoeState,
    _roundsReg :: Registry RoundId RoundState,
    _spotsReg :: Registry SpotId SpotState,
    _spotHandsReg :: Registry HandId HandState
  }
  deriving stock (Generic, Show)

emptyWorldState :: WorldState
emptyWorldState =
  WorldState
    { _offeringsReg = mempty,
      _actorsReg = mempty,
      _shoesReg = mempty,
      _roundsReg = mempty,
      _spotsReg = mempty,
      _spotHandsReg = mempty
    }

-- lenses

lensOfferings :: Lens' WorldState (Registry OfferingId OfferingState)
lensOfferings = lens _offeringsReg (\s x -> s {_offeringsReg = x})

lensActors :: Lens' WorldState (Registry ActorId ActorState)
lensActors = lens _actorsReg (\s x -> s {_actorsReg = x})

lensShoes :: Lens' WorldState (Registry ShoeId ShoeState)
lensShoes = lens _shoesReg (\s x -> s {_shoesReg = x})

lensRounds :: Lens' WorldState (Registry RoundId RoundState)
lensRounds = lens _roundsReg (\s x -> s {_roundsReg = x})

lensSpots :: Lens' WorldState (Registry SpotId SpotState)
lensSpots = lens _spotsReg (\s x -> s {_spotsReg = x})

lensHands :: Lens' WorldState (Registry HandId HandState)
lensHands = lens _spotHandsReg (\s x -> s {_spotHandsReg = x})
