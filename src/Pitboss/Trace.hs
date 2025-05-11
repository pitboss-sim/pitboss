{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Trace where

import Control.Lens (Lens', lens)
import Data.Aeson
import Data.HashMap.Strict.InsOrd qualified as IHM
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Dealer
import Pitboss.Trace.Entity.DealerHand
import Pitboss.Trace.Entity.DealerRound
import Pitboss.Trace.Entity.Offering
import Pitboss.Trace.Entity.Player
import Pitboss.Trace.Entity.PlayerHand
import Pitboss.Trace.Entity.PlayerSpot
import Pitboss.Trace.Entity.Shoe
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.EntityRegistry
import Pitboss.Trace.EntityRegistry.Identifier

-- global simulation state

type DealerHandRegistry = EntityRegistry DealerHandId DealerHandEntity

data Trace = Trace
  { _offeringsEntityRegistry :: EntityRegistry OfferingId OfferingEntity,
    _dealersEntityRegistry :: EntityRegistry DealerId DealerEntity,
    _dealerHandsEntityRegistry :: DealerHandRegistry,
    _playersEntityRegistry :: EntityRegistry PlayerId PlayerEntity,
    _playerHandsEntityRegistry :: EntityRegistry PlayerHandId PlayerHandEntity,
    _shoesEntityRegistry :: EntityRegistry ShoeId ShoeEntity,
    _roundsEntityRegistry :: EntityRegistry RoundId DealerRoundEntity,
    _spotsEntityRegistry :: EntityRegistry PlayerSpotId PlayerSpotEntity,
    _tablesEntityRegistry :: EntityRegistry TableId TableEntity
  }
  deriving (Generic)

instance ToJSON Trace

-- instance ToJSON Trace where
--   toJSON (Trace o d dh p ph s r sp) =
--     object
--       [ "offerings" .= o,
--         "dealers" .= d,
--         "dealerHands" .= dh,
--         "players" .= p,
--         "playerHands" .= ph,
--         "shoes" .= s,
--         "rounds" .= r,
--         "spots" .= sp
--       ]

instance Show Trace where
  show (Trace o d dh p ph s r sp t) =
    unlines
      [ "Trace:",
        "  Offerings: " ++ showKeys o,
        "  Dealers: " ++ showKeys d,
        "  DealerHands: " ++ showKeys dh,
        "  Players: " ++ showKeys p,
        "  PlayerHands: " ++ showKeys ph,
        "  Shoes: " ++ showKeys s,
        "  Rounds: " ++ showKeys r,
        "  Spots: " ++ showKeys sp,
        "  Tables: " ++ showKeys t
      ]
    where
      showKeys = show . IHM.keys

emptyTrace :: Trace
emptyTrace =
  Trace
    { _offeringsEntityRegistry = mempty,
      _dealersEntityRegistry = mempty,
      _dealerHandsEntityRegistry = mempty,
      _playersEntityRegistry = mempty,
      _playerHandsEntityRegistry = mempty,
      _shoesEntityRegistry = mempty,
      _roundsEntityRegistry = mempty,
      _spotsEntityRegistry = mempty,
      _tablesEntityRegistry = mempty
    }

-- lenses

lensOfferings :: Lens' Trace (EntityRegistry OfferingId OfferingEntity)
lensOfferings = lens _offeringsEntityRegistry (\s x -> s {_offeringsEntityRegistry = x})

lensDealers :: Lens' Trace (EntityRegistry DealerId DealerEntity)
lensDealers = lens _dealersEntityRegistry (\s x -> s {_dealersEntityRegistry = x})

lensDealerHands :: Lens' Trace DealerHandRegistry
lensDealerHands = lens _dealerHandsEntityRegistry (\s x -> s {_dealerHandsEntityRegistry = x})

lensPlayers :: Lens' Trace (EntityRegistry PlayerId PlayerEntity)
lensPlayers = lens _playersEntityRegistry (\s x -> s {_playersEntityRegistry = x})

lensPlayerHands :: Lens' Trace (EntityRegistry PlayerId PlayerHandEntity)
lensPlayerHands = lens _playerHandsEntityRegistry (\s x -> s {_playerHandsEntityRegistry = x})

lensShoes :: Lens' Trace (EntityRegistry ShoeId ShoeEntity)
lensShoes = lens _shoesEntityRegistry (\s x -> s {_shoesEntityRegistry = x})

lensRounds :: Lens' Trace (EntityRegistry RoundId DealerRoundEntity)
lensRounds = lens _roundsEntityRegistry (\s x -> s {_roundsEntityRegistry = x})

lensSpots :: Lens' Trace (EntityRegistry PlayerSpotId PlayerSpotEntity)
lensSpots = lens _spotsEntityRegistry (\s x -> s {_spotsEntityRegistry = x})
