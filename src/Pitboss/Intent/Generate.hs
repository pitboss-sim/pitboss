{-# LANGUAGE DataKinds #-}

module Pitboss.Intent.Generate where

import Control.Lens
import Control.Monad.State
import Pitboss.Intent.Context
import Pitboss.Intent.Generate.Mistake
import Pitboss.Intent.Generate.Strategy
import Pitboss.State.Types.Core
import Pitboss.Strategy.Types
import System.Random (StdGen, randomR)
import Pitboss.Agency.Player.Archetype

generatePlayerIntent ::
    PlayerArchetype ->
    GameContext ->
    State StdGen IntentDetails
generatePlayerIntent PerfectPlayer ctx = do
    let move = lookupPerfectStrategy ctx
    pure $ moveToIntentDetails move
generatePlayerIntent (BasicStrategyPlayer mistakes) ctx = do
    gen <- get
    let (shouldMistake, gen') = rollForMistake mistakes gen
    put gen'
    if shouldMistake
        then generateMistake mistakes ctx
        else pure $ moveToIntentDetails (lookupBasicStrategy ctx)
generatePlayerIntent (AdvantagePlayer counting betting devs) ctx = do
    let baseMove = lookupBasicStrategy ctx
        tc = calculateTrueCount counting ctx
        deviatedMove = applyDeviations devs tc baseMove ctx
    applyCoverPlay betting deviatedMove ctx

calculateTrueCount :: CountingConfig -> GameContext -> Int
calculateTrueCount _config ctx =
    let runningCount = ctx ^. contextRunningCount
        decksRemaining = ctx ^. contextDecksRemaining
     in round (fromIntegral runningCount / decksRemaining)

applyDeviations ::
    DeviationConfig ->
    Int ->
    Move ->
    GameContext ->
    Move
applyDeviations _config _tc baseMove _ctx =
    baseMove

applyCoverPlay ::
    BettingConfig ->
    Move ->
    GameContext ->
    State StdGen IntentDetails
applyCoverPlay config move ctx = do
    gen <- get
    let (roll, gen') = randomR (0.0, 1.0) gen
    put gen'
    if roll < config ^. coverBetting . varyBetsRandomly
        then generateCoverMove move ctx
        else pure $ moveToIntentDetails move

generateCoverMove ::
    Move ->
    GameContext ->
    State StdGen IntentDetails
generateCoverMove correctMove _ctx =
    pure $ moveToIntentDetails correctMove
