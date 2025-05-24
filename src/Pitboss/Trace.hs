{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Trace where

import Control.Lens (Lens', lens)
import Data.Aeson
import Data.HashMap.Strict.InsOrd qualified as IHM
import GHC.Generics
import Pitboss.Trace.Entity.Delta
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.Id
import Pitboss.Trace.Registry

type Dealers = Registry (Id 'Dealer) (Delta 'Dealer 'Whole)

type DealerHands = Registry (Id 'DealerHand) (Delta 'DealerHand 'Whole)

type DealerRounds = Registry (Id 'DealerRound) (Delta 'DealerRound 'Whole)

type Offerings = Registry (Id 'Offering) (Delta 'Offering 'Whole)

type Players = Registry (Id 'Player) (Delta 'Player 'Whole)

type PlayerHands = Registry (Id 'PlayerHand) (Delta 'PlayerHand 'Whole)

type PlayerSpots = Registry (Id 'PlayerSpot) (Delta 'PlayerSpot 'Whole)

type Tables = Registry (Id 'Table) (Delta 'DealerHand 'Whole)

type TableShoes = Registry (Id 'TableShoe) (Delta 'DealerHand 'Whole)

data Trace = Trace
    { _offerings :: Offerings
    , _tables :: Tables
    , _tableShoes :: TableShoes
    , _dealers :: Dealers
    , _dealerHands :: DealerHands
    , _dealerRounds :: DealerRounds
    , _players :: Players
    , _playerSpots :: PlayerSpots
    , _playerHands :: PlayerHands
    }
    deriving (Eq, Generic)

instance ToJSON Trace where
    toJSON (Trace o t s d dr dh p ps ph) =
        object
            [ "offerings" .= o
            , "tables" .= t
            , "shoes" .= s
            , "dealers" .= d
            , "dealerRounds" .= dr
            , "dealerHands" .= dh
            , "players" .= p
            , "playerSpots" .= ps
            , "playerHands" .= ph
            ]

instance Show Trace where
    show (Trace o d dh p ph s r sp t) =
        unlines
            [ "Trace:"
            , "  Offerings: " ++ showKeys o
            , "  Dealers: " ++ showKeys d
            , "  DealerHands: " ++ showKeys dh
            , "  Players: " ++ showKeys p
            , "  PlayerHands: " ++ showKeys ph
            , "  TableShoes: " ++ showKeys s
            , "  Rounds: " ++ showKeys r
            , "  Spots: " ++ showKeys sp
            , "  Tables: " ++ showKeys t
            ]
      where
        showKeys = show . IHM.keys . unRegistry

emptyTrace :: Trace
emptyTrace =
    Trace
        { _offerings = mempty
        , _tables = mempty
        , _tableShoes = mempty
        , _dealers = mempty
        , _dealerHands = mempty
        , _dealerRounds = mempty
        , _players = mempty
        , _playerSpots = mempty
        , _playerHands = mempty
        }

-- lenses

lensOfferings :: Lens' Trace Offerings
lensOfferings = lens _offerings (\s x -> s{_offerings = x})

lensTables :: Lens' Trace Tables
lensTables = lens _tables (\s x -> s{_tables = x})

lensTableShoes :: Lens' Trace TableShoes
lensTableShoes = lens _tableShoes (\s x -> s{_tableShoes = x})

lensDealers :: Lens' Trace Dealers
lensDealers = lens _dealers (\s x -> s{_dealers = x})

lensDealerRounds :: Lens' Trace DealerRounds
lensDealerRounds = lens _dealerRounds (\s x -> s{_dealerRounds = x})

lensDealerHands :: Lens' Trace DealerHands
lensDealerHands = lens _dealerHands (\s x -> s{_dealerHands = x})

lensPlayers :: Lens' Trace Players
lensPlayers = lens _players (\s x -> s{_players = x})

lensPlayerSpots :: Lens' Trace PlayerSpots
lensPlayerSpots = lens _playerSpots (\s x -> s{_playerSpots = x})

lensPlayerHands :: Lens' Trace PlayerHands
lensPlayerHands = lens _playerHands (\s x -> s{_playerHands = x})
