{-# LANGUAGE DataKinds #-}
module Pitboss.Agency.Archetype.Player.BasicStrategy where

import Control.Lens
import Control.Monad.State
import Pitboss.Agency.Archetype.Types
import Pitboss.Agency.Types
import Pitboss.Blackjack.BasicStrategy.Chart.Interpret (safeDecisionLookup)
import Pitboss.Blackjack.BasicStrategy.Types (Decision (..), Fallback (..))
import Pitboss.Blackjack.BasicStrategy.Types qualified as BS
import Pitboss.Blackjack.Materia.Card (rank)
import System.Random

getBasicStrategyMove :: ArchetypeConfig 'BasicStrategy -> GameContext -> State StdGen Move
getBasicStrategyMove config ctx = do
    let chart = bcStrategyChart config
        hand = _contextPlayerHand ctx
        upcard = rank (_contextDealerUpcard ctx)
        offering = _contextOffering ctx
        decision = safeDecisionLookup chart hand upcard offering
        baseMove = decisionToMove decision

    shouldMistake <- rollForMistake (bcMistakeProfile config)
    if shouldMistake
        then generateMistake (bcMistakeProfile config) ctx baseMove
        else pure baseMove

decisionToMove :: Decision -> Move
decisionToMove (Always move) = bsToMove move
decisionToMove (Prefer primary (Else _)) = bsToMove primary

bsToMove :: BS.Move -> Move
bsToMove BS.Hit = Hit
bsToMove BS.Stand = Stand
bsToMove BS.Double = Double
bsToMove BS.Split = Split
bsToMove BS.Surrender = Surrender

rollForMistake :: MistakeProfile -> State StdGen Bool
rollForMistake profile = do
    gen <- get
    let rate = profile ^. mistakeRate
        (roll, gen') = randomR (0.0, 1.0) gen
    put gen'
    pure (roll < rate)

generateMistake :: MistakeProfile -> GameContext -> Move -> State StdGen Move
generateMistake profile ctx correct = do
    gen <- get
    let dist = profile ^. mistakeDistribution
        (roll, gen') = randomR (0.0, 1.0) gen
    put gen'
    pure $ selectMistakeType dist roll correct ctx

selectMistakeType :: MistakeDistribution -> Double -> Move -> GameContext -> Move
selectMistakeType dist roll correct ctx =
    case correct of
        Stand | roll < dist ^. hitInsteadOfStand -> Hit
        Hit | roll < dist ^. standInsteadOfHit -> Stand
        Double
            | roll < dist ^. noDoubleWhenShould ->
                if ctx ^. contextCanDouble then Hit else Stand
        Split
            | roll < dist ^. noSplitWhenShould -> Hit
        _
            | roll < dist ^. doubleWhenShouldnt && ctx ^. contextCanDouble -> Double
        _
            | roll < dist ^. splitWhenShouldnt && ctx ^. contextCanSplit -> Split
        _ -> correct
