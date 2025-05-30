{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Pitboss.Agency.Archetype.Player.Strategy (
    module Pitboss.Agency.Archetype.Player.Strategy.Types,
    module Pitboss.Agency.Archetype.Player.Strategy.Basic,
    module Pitboss.Agency.Archetype.Player.Strategy.Mistake,
    module Pitboss.Agency.Archetype.Player.Strategy.Advantage,
    getPlayerMove,
) where

import Control.Monad.State
import Pitboss.Agency.Archetype.Player.Strategy.Advantage
import Pitboss.Agency.Archetype.Player.Strategy.Basic
import Pitboss.Agency.Archetype.Player.Strategy.Mistake
import Pitboss.Agency.Archetype.Player.Strategy.Types
import Pitboss.Agency.Archetype.Types
import Pitboss.Blackjack.BasicStrategy.Types
import System.Random

getPlayerMove ::
    SomePlayerArchetype ->
    GameContext ->
    State StdGen Move
getPlayerMove archetype ctx = case archetype of
    SomePlayerPerfect _ ->
        pure $ lookupPerfectStrategy ctx
    SomePlayerBasicStrategy (BasicStrategyArchetype{..}) ->
        let BasicConfig{..} = bsConfig
         in do
                gen <- get
                let (shouldMistake, gen') = rollForMistake bcMistakeProfile gen
                put gen'
                if shouldMistake
                    then generateMistake bcMistakeProfile ctx
                    else pure $ lookupBasicStrategy ctx
    SomePlayerAdvantage (AdvantageArchetype{..}) ->
        let AdvantageConfig{..} = advConfig
            AdvantageState{..} = advState
         in do
                let baseMove = lookupBasicStrategy ctx
                    tc = round asTrueCount :: Int
                    deviatedMove = applyDeviations acBettingSpread tc baseMove ctx
                applyCoverPlay acBettingSpread deviatedMove ctx
    SomePlayerSuperstitious (SuperstitiousArchetype{}) ->
        pure $ lookupBasicStrategy ctx -- For now, treat like basic strategy
