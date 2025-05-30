module Pitboss.Intent.Generate.Mistake where

import Control.Lens
import Control.Monad.State
import Pitboss.Archetype.Player
import Pitboss.Intent.Context
import Pitboss.Intent.Generate.Strategy
import Pitboss.State.Types.Core
import Pitboss.Blackjack.BasicStrategy.Types
import System.Random

generateMistake ::
    MistakeProfile ->
    GameContext ->
    State StdGen IntentDetails
generateMistake profile ctx = do
    let correct = lookupBasicStrategy ctx
        dist = profile ^. mistakeTypes
    gen <- get
    let (roll, gen') = randomR (0.0, 1.0) gen
    put gen'

    let mistake = selectMistakeType dist roll correct ctx
    pure $ moveToIntentDetails mistake

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
