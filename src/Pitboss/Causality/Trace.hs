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
    , _dealers :: Registry 'Dealer (SomeDelta 'Dealer)
    , _players :: Registry 'Player (SomeDelta 'Player)
    , _rounds :: Registry 'Round (SomeDelta 'Round)
    , _shoes :: Registry 'Shoe (SomeDelta 'Shoe)
    , _tables :: Registry 'Table (SomeDelta 'Table)
    }
    deriving (Eq, Show, Generic)

instance ToJSON Trace where
    toJSON (Trace b d p r s t) =
        object
            [ "bouts" .= b
            , "dealers" .= d
            , "players" .= p
            , "rounds" .= r
            , "shoes" .= s
            , "tables" .= t
            ]

instance FromJSON Trace where
    parseJSON = withObject "Trace" $ \o ->
        Trace
            <$> o .: "bouts"
            <*> o .: "dealers"
            <*> o .: "players"
            <*> o .: "rounds"
            <*> o .: "shoes"
            <*> o .: "tables"

emptyTrace :: Trace
emptyTrace =
    Trace
        { _bouts = mempty
        , _dealers = mempty
        , _players = mempty
        , _rounds = mempty
        , _shoes = mempty
        , _tables = mempty
        }

makeLenses ''Trace
