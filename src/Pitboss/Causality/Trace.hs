{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Pitboss.Causality.Trace where

import Control.Lens (makeLenses)
import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Registry
import Pitboss.Causality.Types.Core

data Trace = Trace
    { _bouts :: Registry 'Bout (SomeDelta 'Bout)
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
    toJSON (Trace b t s d dh dr p ps ph) =
        object
            [ "bouts" .= b
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
            <$> o .: "bouts"
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
        { _bouts = mempty
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
