{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Pitboss.State.Trace (
    emptyTrace,
) where

import Control.Lens (makeLenses)
import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.State.Delta.Types
import Pitboss.State.Registry
import Pitboss.State.Types.Core

data Trace = Trace
    { _intents :: Registry 'Intent (SomeDelta 'Intent)
    , _events :: Registry 'Event (SomeDelta 'Event)
    , _bouts :: Registry 'Bout (SomeDelta 'Bout)
    , _offerings :: Registry 'Offering (SomeDelta 'Offering)
    , _tables :: Registry 'Table (SomeDelta 'Table)
    , _tableShoes :: Registry 'TableShoe (SomeDelta 'TableShoe)
    , _dealers :: Registry 'Dealer (SomeDelta 'Dealer)
    , _dealerHands :: Registry 'DealerHand (SomeDelta 'DealerHand)
    , _dealerRounds :: Registry 'DealerRound (SomeDelta 'DealerRound)
    , _players :: Registry 'Player (SomeDelta 'Player)
    , _playerSpots :: Registry 'PlayerSpot (SomeDelta 'PlayerSpot)
    , _playerHands :: Registry 'PlayerHand (SomeDelta 'PlayerHand)
    }
    deriving (Eq, Generic)

instance ToJSON Trace where
    toJSON (Trace i e b o t s d dh dr p ps ph) =
        object
            [ "intents" .= i
            , "events" .= e
            , "bouts" .= b
            , "offerings" .= o
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
            <$> o .: "intents"
            <*> o .: "events"
            <*> o .: "bouts"
            <*> o .: "offerings"
            <*> o .: "tables"
            <*> o .: "shoes"
            <*> o .: "dealers"
            <*> o .: "dealerHands"
            <*> o .: "dealerRounds"
            <*> o .: "players"
            <*> o .: "playerSpots"
            <*> o .: "playerHands"

emptyTrace :: Trace
emptyTrace =
    Trace
        { _intents = mempty
        , _events = mempty
        , _bouts = mempty
        , _offerings = mempty
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
