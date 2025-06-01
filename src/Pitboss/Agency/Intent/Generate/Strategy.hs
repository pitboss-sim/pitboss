{-# LANGUAGE DataKinds #-}

module Pitboss.Agency.Intent.Generate.Strategy where

import Control.Monad.State
import Pitboss.Agency.Archetype.Player.Strategy
import Pitboss.Agency.Archetype.Types
import Pitboss.Agency.Intent.Types
import Pitboss.Blackjack.BasicStrategy.Types
import System.Random

moveToIntentKind :: Move -> IntentKind
moveToIntentKind Hit = IPlayerHit
moveToIntentKind Stand = IPlayerStand
moveToIntentKind Double = IPlayerDouble
moveToIntentKind Split = IPlayerSplit
moveToIntentKind Surrender = IPlayerSurrender

generatePlayerIntent ::
    SomePlayerArchetype ->
    GameContext ->
    State StdGen IntentKind
generatePlayerIntent archetype ctx = do
    move <- getPlayerMove archetype ctx
    pure $ moveToIntentKind move
