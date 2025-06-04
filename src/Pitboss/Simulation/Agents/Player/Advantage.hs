{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Agents.Player.Advantage where

import Control.Monad.State
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Pitboss.Blackjack
import Pitboss.Simulation.Agents.Types
import System.Random

getAdvantageMove :: ArchetypeConfig 'Advantage -> ArchetypeState 'Advantage -> BoutContext -> State StdGen Move
getAdvantageMove config state' ctx = do
    let tc = round (asTrueCount state') :: Int
        baseMove = calculateBaseMove ctx
        deviatedMove = applyDeviations (acDeviationChart config) tc baseMove ctx
    applyCoverPlay (acBettingSpread config) deviatedMove ctx

calculateBaseMove :: BoutContext -> Move
calculateBaseMove ctx =
    let hand = _contextBoutPlayerHand ctx
        rules = gameRuleSet (_contextOffering ctx)
        availMoves = availableMoves hand rules 0
        score = handScore hand
     in if score < 17 && MHit `elem` availMoves
            then MHit
            else MStand

applyDeviations :: DeviationChart -> Int -> Move -> BoutContext -> Move
applyDeviations (DeviationChart deviationMap) trueCount baseMove ctx =
    case Map.lookup trueCount deviationMap of
        Nothing -> baseMove
        Just deviations ->
            case findApplicableDeviation deviations ctx of
                Nothing -> baseMove
                Just deviation -> applyDeviation deviation baseMove

findApplicableDeviation :: [Deviation] -> BoutContext -> Maybe Deviation
findApplicableDeviation deviations ctx =
    let playerTotal = handScore (_contextBoutPlayerHand ctx)
     in listToMaybe [d | d <- deviations, devPlayerTotal d == playerTotal]

applyDeviation :: Deviation -> Move -> Move
applyDeviation (Deviation _ _ action) _ =
    case action of
        DevStandInsteadOfHit -> MStand
        DevHitInsteadOfStand -> MHit
        DevDoubleInsteadOfHit -> MDouble

applyCoverPlay :: BettingSpread -> Move -> BoutContext -> State StdGen Move
applyCoverPlay spread move _ = do
    case spread of
        FlatBetting -> pure move
        SimpleSpread _ _ -> pure move
        TrueCountSpread _ -> pure move
