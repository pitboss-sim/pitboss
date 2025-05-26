{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pitboss.State.Trace (
    emptyTrace,
) where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.HashMap.Strict.InsOrd qualified as IHM
import GHC.Generics (Generic)
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Registry

data Trace = Trace
    { _offerings :: Registry 'Offering (Delta 'Offering 'TransactionBoundary)
    , _tables :: Registry 'Table (Delta 'Table 'TransactionBoundary)
    , _tableShoes :: Registry 'TableShoe (Delta 'TableShoe 'TransactionBoundary)
    , _dealers :: Registry 'Dealer (Delta 'Dealer 'TransactionBoundary)
    , _dealerHands :: Registry 'DealerHand (Delta 'DealerHand 'TransactionBoundary)
    , _dealerRounds :: Registry 'DealerRound (Delta 'DealerRound 'TransactionBoundary)
    , _players :: Registry 'Player (Delta 'Player 'TransactionBoundary)
    , _playerSpots :: Registry 'PlayerSpot (Delta 'PlayerSpot 'TransactionBoundary)
    , _playerHands :: Registry 'PlayerHand (Delta 'PlayerHand 'TransactionBoundary)
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

makeLenses ''Trace
