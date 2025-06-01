{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Pitboss.Agency.Archetype.Player.Strategy.Types where

import Control.Lens
import Pitboss.Blackjack.Materia.Card
import Pitboss.Blackjack.Materia.Hand
import Pitboss.Blackjack.Offering
import Pitboss.Blackjack.Play (canDoubleSomeHand, canSplitSomeHand)

data GameContext = GameContext
    { _contextPlayerHand :: SomeHand
    , _contextDealerUpcard :: Card
    , _contextOffering :: Offering
    , _contextCanDouble :: Bool
    , _contextCanSplit :: Bool
    , _contextCanSurrender :: Bool
    , _contextRunningCount :: Int
    , _contextDecksRemaining :: Double
    , _contextHandNumber :: Int
    , _contextSplitCount :: Int
    }
    deriving (Eq, Show)

makeLenses ''GameContext

buildGameContext ::
    SomeHand ->
    Card ->
    Offering ->
    Int ->
    Double ->
    GameContext
buildGameContext hand upcard offering count decks =
    GameContext
        { _contextPlayerHand = hand
        , _contextDealerUpcard = upcard
        , _contextOffering = offering
        , _contextCanDouble = canDoubleSomeHand hand offering
        , _contextCanSplit = canSplitSomeHand hand 0 offering
        , _contextCanSurrender = True
        , _contextRunningCount = count
        , _contextDecksRemaining = decks
        , _contextHandNumber = 1
        , _contextSplitCount = 0
        }
