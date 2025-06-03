{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Pitboss.Sim.Agency.Types where

import Control.Lens
import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Blackjack

data GameContext = GameContext
    { _contextPlayerHand :: SomeHand
    , _contextDealerUpcard :: Card
    , _contextOffering :: Offering
    , _contextCanDouble :: Bool
    , _contextCanSplit :: Bool
    , _contextCanSurrender :: Bool
    , _contextHandNumber :: Int
    , _contextSplitCount :: Int
    }
    deriving (Eq, Show, Generic)

makeLenses ''GameContext

instance ToJSON GameContext
instance FromJSON GameContext
