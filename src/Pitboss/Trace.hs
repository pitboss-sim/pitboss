{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Trace where

import Control.Lens (Lens', lens)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Dealer
import Pitboss.Trace.Entity.DealerHand
import Pitboss.Trace.Entity.Offering
import Pitboss.Trace.Entity.Player
import Pitboss.Trace.Entity.PlayerHand
import Pitboss.Trace.Entity.Round
import Pitboss.Trace.Entity.Spot
import Pitboss.Trace.Timeline
import Pitboss.Trace.Timeline.Identifier

-- global simulation state

data Trace = Trace
  { _offeringsTimeline :: Timeline OfferingId OfferingState,
    _dealersTimeline :: Timeline DealerId DealerState,
    _dealerHandsTimeline :: Timeline DealerId DealerHandState,
    _playersTimeline :: Timeline PlayerId PlayerState,
    _playerHandsTimeline :: Timeline PlayerId PlayerHandState,
    _shoesTimeline :: Timeline ShoeId SpotState,
    _roundsTimeline :: Timeline RoundId RoundState,
    _spotsTimeline :: Timeline SpotId SpotState
  }
  deriving stock (Generic, Show)

emptyTrace :: Trace
emptyTrace =
  Trace
    { _offeringsTimeline = mempty,
      _dealersTimeline = mempty,
      _dealerHandsTimeline = mempty,
      _playersTimeline = mempty,
      _playerHandsTimeline = mempty,
      _shoesTimeline = mempty,
      _roundsTimeline = mempty,
      _spotsTimeline = mempty
    }

-- lenses

lensOfferings :: Lens' Trace (Timeline OfferingId OfferingState)
lensOfferings = lens _offeringsTimeline (\s x -> s {_offeringsTimeline = x})

lensDealers :: Lens' Trace (Timeline DealerId DealerState)
lensDealers = lens _dealersTimeline (\s x -> s {_dealersTimeline = x})

lensDealerHands :: Lens' Trace (Timeline DealerId DealerHandState)
lensDealerHands = lens _dealerHandsTimeline (\s x -> s {_dealerHandsTimeline = x})

lensPlayers :: Lens' Trace (Timeline PlayerId PlayerState)
lensPlayers = lens _playersTimeline (\s x -> s {_playersTimeline = x})

lensPlayerHands :: Lens' Trace (Timeline PlayerId PlayerHandState)
lensPlayerHands = lens _playerHandsTimeline (\s x -> s {_playerHandsTimeline = x})

lensShoes :: Lens' Trace (Timeline ShoeId SpotState)
lensShoes = lens _shoesTimeline (\s x -> s {_shoesTimeline = x})

lensRounds :: Lens' Trace (Timeline Bounded RoundState)
lensRounds = lens _roundsTimeline (\s x -> s {_roundsTimeline = x})

lensSpots :: Lens' Trace (Timeline SpotId SpotState)
lensSpots = lens _spotsTimeline (\s x -> s {_spotsTimeline = x})
