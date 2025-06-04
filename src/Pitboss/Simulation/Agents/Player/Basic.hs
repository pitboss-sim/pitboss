{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Agents.Player.Basic where

import Control.Lens
import Control.Monad.State
import Pitboss.Blackjack
import Pitboss.Simulation.Agents.Types
import System.Random

getBasicStrategyMove :: ArchetypeConfig 'BasicStrategy -> BoutContext -> State StdGen Move
getBasicStrategyMove config ctx = do
    let chart = bcStrategyChart config
        boutPlayerHand = _contextBoutPlayerHand ctx
        upcard = rank (_contextDealerUpcard ctx)
        offering = _contextOffering ctx
        decision = safeDecisionLookup chart boutPlayerHand upcard offering
        rules = gameRuleSet offering

    let baseMove = case decision of
            Always move -> move
            Prefer primary (Else fallback) ->
                if isLegalMove primary boutPlayerHand rules
                    then primary
                    else fallback

    shouldMistake <- rollForMistake (bcMistakeProfile config)
    if shouldMistake
        then generateMistake (bcMistakeProfile config) ctx baseMove
        else pure baseMove

rollForMistake :: MistakeProfile -> State StdGen Bool
rollForMistake profile = do
    roll <- state $ randomR (0.0, 1.0)
    pure (roll < profile ^. mistakeRate)

generateMistake :: MistakeProfile -> BoutContext -> Move -> State StdGen Move
generateMistake profile ctx correct = do
    roll <- state $ randomR (0.0, 1.0)
    pure $ selectMistakeType (profile ^. mistakeDistribution) roll correct ctx

selectMistakeType :: MistakeDistribution -> Double -> Move -> BoutContext -> Move
selectMistakeType dist roll correct ctx =
    let rules = gameRuleSet (_contextOffering ctx)
        hand = _contextBoutPlayerHand ctx
        splitCount = 0
        availMoves = availableMoves hand rules splitCount
     in case correct of
            MStand | roll < dist ^. hitInsteadOfStand && MHit `elem` availMoves -> MHit
            MHit | roll < dist ^. standInsteadOfHit -> MStand
            MDouble | roll < dist ^. noDoubleWhenShould && MHit `elem` availMoves -> MHit
            MSplit | roll < dist ^. noSplitWhenShould && MHit `elem` availMoves -> MHit
            _
                | roll < dist ^. doubleWhenShouldnt && MDouble `elem` availMoves -> MDouble
                | roll < dist ^. splitWhenShouldnt && MSplit `elem` availMoves -> MSplit
                | otherwise -> correct
