module Pitboss.Agency.Archetype.Player.Strategy.Advantage where

import Control.Lens
import Control.Monad.State
import Pitboss.Agency.Archetype.Player
import Pitboss.Agency.Archetype.Player.Strategy.Types
import Pitboss.Blackjack.BasicStrategy.Types
import System.Random

calculateTrueCount :: CountingConfig -> GameContext -> Int
calculateTrueCount _config ctx =
    let runningCount = ctx ^. contextRunningCount
        decksRemaining = ctx ^. contextDecksRemaining
     in round (fromIntegral runningCount / decksRemaining)

applyDeviations ::
    BettingSpread ->
    Int ->
    Move ->
    GameContext ->
    Move
applyDeviations _config _tc baseMove _ctx =
    baseMove

applyCoverPlay ::
    BettingSpread ->
    Move ->
    GameContext ->
    State StdGen Move
applyCoverPlay _config move _ctx =
    pure move
