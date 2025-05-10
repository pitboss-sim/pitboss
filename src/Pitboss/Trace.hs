{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Trace where

import Control.Lens (Lens', lens)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Actor
import Pitboss.Trace.Entity.Hand
import Pitboss.Trace.Entity.Offering
import Pitboss.Trace.Entity.Spot
import Pitboss.Trace.Timeline
import Pitboss.Trace.Timeline.Identifier

-- global simulation state

data Trace = Trace
  { _offeringsTimeline :: Timeline OfferingId OfferingState,
    _actorsTimeline :: Timeline ActorId ActorState,
    _shoesTimeline :: Timeline ShoeId SpotState,
    _roundsTimeline :: Timeline RoundId HandState,
    _spotsTimeline :: Timeline SpotId SpotState,
    _spotHandsTimeline :: Timeline HandId HandState
  }
  deriving stock (Generic, Show)

emptyTrace :: Trace
emptyTrace =
  Trace
    { _offeringsTimeline = mempty,
      _actorsTimeline = mempty,
      _shoesTimeline = mempty,
      _roundsTimeline = mempty,
      _spotsTimeline = mempty,
      _spotHandsTimeline = mempty
    }

-- lenses

lensOfferings :: Lens' Trace (Timeline OfferingId OfferingState)
lensOfferings = lens _offeringsTimeline (\s x -> s {_offeringsTimeline = x})

lensActors :: Lens' Trace (Timeline ActorId ActorState)
lensActors = lens _actorsTimeline (\s x -> s {_actorsTimeline = x})

lensShoes :: Lens' Trace (Timeline ShoeId SpotState)
lensShoes = lens _shoesTimeline (\s x -> s {_shoesTimeline = x})

lensRounds :: Lens' Trace (Timeline Bounded HandState)
lensRounds = lens _roundsTimeline (\s x -> s {_roundsTimeline = x})

lensSpots :: Lens' Trace (Timeline SpotId SpotState)
lensSpots = lens _spotsTimeline (\s x -> s {_spotsTimeline = x})

lensHands :: Lens' Trace (Timeline HandId HandState)
lensHands = lens _spotHandsTimeline (\s x -> s {_spotHandsTimeline = x})
