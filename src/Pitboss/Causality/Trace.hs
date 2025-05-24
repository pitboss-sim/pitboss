{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Pitboss.Causality.Trace where

import Control.Lens (makeLenses)
import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Registry
import Pitboss.Causality.Types.Core

data Trace = Trace
    { _bouts :: Registry 'Bout (SomeDelta 'Bout)
    , _contestants :: Registry 'Contestant (SomeDelta 'Contestant)
    , _dealers :: Registry 'Dealer (SomeDelta 'Dealer)
    , _hands :: Registry 'Hand (SomeDelta 'Hand)
    , _players :: Registry 'Player (SomeDelta 'Player)
    , _rounds :: Registry 'Round (SomeDelta 'Round)
    , _shoes :: Registry 'Shoe (SomeDelta 'Shoe)
    , _tables :: Registry 'Table (SomeDelta 'Table)
    }
    deriving (Eq, Generic)

instance ToJSON Trace where
    toJSON (Trace b bp d dr h p ts t) =
        object
            [ "bouts" .= b
            , "contestants" .= bp
            , "dealers" .= d
            , "hands" .= h
            , "players" .= p
            , "rounds" .= dr
            , "shoes" .= ts
            , "tables" .= t
            ]

instance FromJSON Trace where
    parseJSON = withObject "Trace" $ \o ->
        Trace
            <$> o .: "bouts"
            <*> o .: "contestants"
            <*> o .: "dealers"
            <*> o .: "hands"
            <*> o .: "players"
            <*> o .: "rounds"
            <*> o .: "shoes"
            <*> o .: "tables"

emptyTrace :: Trace
emptyTrace =
    Trace
        { _bouts = mempty
        , _contestants = mempty
        , _dealers = mempty
        , _hands = mempty
        , _players = mempty
        , _rounds = mempty
        , _shoes = mempty
        , _tables = mempty
        }

makeLenses ''Trace
