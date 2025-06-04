{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Agents.Player.Basic where

import Control.Lens
import Control.Monad.State
import Pitboss.Blackjack
import Pitboss.Blackjack.Strategy.Chart
import Pitboss.Blackjack.Strategy.Types
import Pitboss.Simulation.Agents.Types
import System.Random

getBasicStrategyMove :: ArchetypeConfig 'BasicStrategy -> GameContext -> State StdGen Move
getBasicStrategyMove config ctx = do
    let chart = bcStrategyChart config
        hand = _contextPlayerHand ctx
        upcard = rank (_contextDealerUpcard ctx)
        offering = _contextOffering ctx
        decision = safeDecisionLookup chart hand upcard offering

    let baseMove = case decision of
            Always move -> move
            Prefer primary (Else fallback) ->
                if isLegalMove primary ctx then primary else fallback

    shouldMistake <- rollForMistake (bcMistakeProfile config)
    if shouldMistake
        then generateMistake (bcMistakeProfile config) ctx baseMove
        else pure baseMove

isLegalMove :: Move -> GameContext -> Bool
isLegalMove Double ctx = _contextCanDouble ctx
isLegalMove Split ctx = _contextCanSplit ctx
isLegalMove Surrender ctx = _contextCanSurrender ctx
isLegalMove _ _ = True

rollForMistake :: MistakeProfile -> State StdGen Bool
rollForMistake profile = do
    roll <- state $ randomR (0.0, 1.0)
    pure (roll < profile ^. mistakeRate)

generateMistake :: MistakeProfile -> GameContext -> Move -> State StdGen Move
generateMistake profile ctx correct = do
    roll <- state $ randomR (0.0, 1.0)
    pure $ selectMistakeType (profile ^. mistakeDistribution) roll correct ctx

selectMistakeType :: MistakeDistribution -> Double -> Move -> GameContext -> Move
selectMistakeType dist roll correct ctx = case correct of
    Stand | roll < dist ^. hitInsteadOfStand -> Hit
    Hit | roll < dist ^. standInsteadOfHit -> Stand
    Double | roll < dist ^. noDoubleWhenShould -> Hit
    Split | roll < dist ^. noSplitWhenShould -> Hit
    _
        | roll < dist ^. doubleWhenShouldnt && _contextCanDouble ctx -> Double
        | roll < dist ^. splitWhenShouldnt && _contextCanSplit ctx -> Split
        | otherwise -> correct
