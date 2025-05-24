{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.State.Trace (
    emptyTrace,
    lensOfferings,
    lensTables,
    lensTableShoes,
    lensDealers,
    lensDealerRounds,
    lensDealerHands,
    lensPlayers,
    lensPlayerSpots,
    lensPlayerHands,
) where

import Control.Lens (Lens', lens)
import Data.Aeson
import Data.HashMap.Strict.InsOrd qualified as IHM
import GHC.Generics (Generic)
import Pitboss.State.Entity.Types
import Pitboss.State.Registry

data Trace = Trace
    { _offerings :: Registry 'Offering (EntityState 'Offering 'Whole)
    , _tables :: Registry 'Table (EntityState 'Table 'Whole)
    , _tableShoes :: Registry 'TableShoe (EntityState 'TableShoe 'Whole)
    , _dealers :: Registry 'Dealer (EntityState 'Dealer 'Whole)
    , _dealerHands :: Registry 'DealerHand (EntityState 'DealerHand 'Whole)
    , _dealerRounds :: Registry 'DealerRound (EntityState 'DealerRound 'Whole)
    , _players :: Registry 'Player (EntityState 'Player 'Whole)
    , _playerSpots :: Registry 'PlayerSpot (EntityState 'PlayerSpot 'Whole)
    , _playerHands :: Registry 'PlayerHand (EntityState 'PlayerHand 'Whole)
    }
    deriving (Eq, Generic)

instance ToJSON Trace where
    toJSON (Trace o t s d dh dr p ps ph) =
        object
            [ "offerings" .= o
            , "tables" .= t
            , "shoes" .= s
            , "dealers" .= d
            , "dealerHands" .= dh
            , "dealerRounds" .= dr
            , "players" .= p
            , "playerSpots" .= ps
            , "playerHands" .= ph
            ]

instance FromJSON Trace where
    parseJSON = withObject "Trace" $ \o ->
        Trace
            <$> o .: "offerings"
            <*> o .: "tables"
            <*> o .: "shoes"
            <*> o .: "dealers"
            <*> o .: "dealerHands"
            <*> o .: "dealerRounds"
            <*> o .: "players"
            <*> o .: "playerSpots"
            <*> o .: "playerHands"

instance Show Trace where
    show (Trace o t s d dh dr p ps ph) =
        unlines
            [ "Trace:"
            , "  Offerings: " ++ showKeys o
            , "  Tables: " ++ showKeys t
            , "  TableShoes: " ++ showKeys s
            , "  Dealers: " ++ showKeys d
            , "  DealerHands: " ++ showKeys dh
            , "  DealerRounds: " ++ showKeys dr
            , "  Players: " ++ showKeys p
            , "  PlayerSpots: " ++ showKeys ps
            , "  PlayerHands: " ++ showKeys ph
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

lensDealers :: Lens' Trace (Registry 'Dealer (EntityState 'Dealer 'Whole))
lensDealers = lens _dealers (\s x -> s{_dealers = x})

lensDealerHands :: Lens' Trace (Registry 'DealerHand (EntityState 'DealerHand 'Whole))
lensDealerHands = lens _dealerHands (\s x -> s{_dealerHands = x})

lensDealerRounds :: Lens' Trace (Registry 'DealerRound (EntityState 'DealerRound 'Whole))
lensDealerRounds = lens _dealerRounds (\s x -> s{_dealerRounds = x})

lensOfferings :: Lens' Trace (Registry 'Offering (EntityState 'Offering 'Whole))
lensOfferings = lens _offerings (\s x -> s{_offerings = x})

lensPlayers :: Lens' Trace (Registry 'Player (EntityState 'Player 'Whole))
lensPlayers = lens _players (\s x -> s{_players = x})

lensPlayerHands :: Lens' Trace (Registry 'PlayerHand (EntityState 'PlayerHand 'Whole))
lensPlayerHands = lens _playerHands (\s x -> s{_playerHands = x})

lensPlayerSpots :: Lens' Trace (Registry 'PlayerSpot (EntityState 'PlayerSpot 'Whole))
lensPlayerSpots = lens _playerSpots (\s x -> s{_playerSpots = x})

lensTables :: Lens' Trace (Registry 'Table (EntityState 'Table 'Whole))
lensTables = lens _tables (\s x -> s{_tables = x})

lensTableShoes :: Lens' Trace (Registry 'TableShoe (EntityState 'TableShoe 'Whole))
lensTableShoes = lens _tableShoes (\s x -> s{_tableShoes = x})
