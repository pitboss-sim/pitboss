{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Trace where

import Control.Lens (Lens', lens)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Actor
import Pitboss.Trace.Entity.Hand
import Pitboss.Trace.Entity.Offering
import Pitboss.Trace.Entity.Spot
import Pitboss.Trace.Registry
import Pitboss.Trace.Registry.Identifier

-- global simulation state

data Trace = Trace
  { _offeringsReg :: Registry OfferingId OfferingState,
    _actorsReg :: Registry ActorId ActorState,
    _shoesReg :: Registry ShoeId SpotState,
    _roundsReg :: Registry RoundId HandState,
    _spotsReg :: Registry SpotId SpotState,
    _spotHandsReg :: Registry HandId HandState
  }
  deriving stock (Generic, Show)

emptyTrace :: Trace
emptyTrace =
  Trace
    { _offeringsReg = mempty,
      _actorsReg = mempty,
      _shoesReg = mempty,
      _roundsReg = mempty,
      _spotsReg = mempty,
      _spotHandsReg = mempty
    }

-- lenses

lensOfferings :: Lens' Trace (Registry OfferingId OfferingState)
lensOfferings = lens _offeringsReg (\s x -> s {_offeringsReg = x})

lensActors :: Lens' Trace (Registry ActorId ActorState)
lensActors = lens _actorsReg (\s x -> s {_actorsReg = x})

lensShoes :: Lens' Trace (Registry ShoeId SpotState)
lensShoes = lens _shoesReg (\s x -> s {_shoesReg = x})

lensRounds :: Lens' Trace (Registry Bounded HandState)
lensRounds = lens _roundsReg (\s x -> s {_roundsReg = x})

lensSpots :: Lens' Trace (Registry SpotId SpotState)
lensSpots = lens _spotsReg (\s x -> s {_spotsReg = x})

lensHands :: Lens' Trace (Registry HandId HandState)
lensHands = lens _spotHandsReg (\s x -> s {_spotHandsReg = x})
