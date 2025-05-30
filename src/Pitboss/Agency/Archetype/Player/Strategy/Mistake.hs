module Pitboss.Agency.Archetype.Player.Strategy.Mistake where

import Control.Lens
import Control.Monad.State
import Pitboss.Agency.Archetype.Player
import Pitboss.Agency.Archetype.Player.Strategy.Basic
import Pitboss.Agency.Archetype.Player.Strategy.Types
import Pitboss.Blackjack.BasicStrategy.Types
import System.Random

generateMistake ::
    MistakeProfile ->
    GameContext ->
    State StdGen Move
generateMistake profile ctx = do
    let correct = lookupBasicStrategy ctx
        dist = profile ^. mistakeDistribution
    gen <- get
    let (roll, gen') = randomR (0.0, 1.0) gen
    put gen'

    pure $ selectMistakeType dist roll correct ctx

selectMistakeType ::
    MistakeDistribution ->
    Double ->
    Move ->
    GameContext ->
    Move
selectMistakeType dist roll correct ctx =
    case correct of
        Stand | roll < dist ^. hitInsteadOfStand -> Hit
        Hit | roll < dist ^. standInsteadOfHit -> Stand
        Double
            | roll < dist ^. noDoubleWhenShould ->
                if ctx ^. contextCanDouble then Hit else Stand
        Split
            | roll < dist ^. noSplitWhenShould ->
                Hit
        _
            | roll < dist ^. doubleWhenShouldnt && ctx ^. contextCanDouble ->
                Double
        _
            | roll < dist ^. splitWhenShouldnt && ctx ^. contextCanSplit ->
                Split
        _ -> correct

rollForMistake :: MistakeProfile -> StdGen -> (Bool, StdGen)
rollForMistake profile gen =
    let rate = profile ^. mistakeRate
        (roll, gen') = randomR (0.0, 1.0) gen
     in (roll < rate, gen')
