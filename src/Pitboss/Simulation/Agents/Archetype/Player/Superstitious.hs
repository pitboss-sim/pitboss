{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Simulation.Agents.Archetype.Player.Superstitious where

import Control.Monad.State
import Pitboss.Blackjack
import Pitboss.Blackjack.Strategy.Chart
import Pitboss.Blackjack.Strategy.Types
import Pitboss.Simulation.Agents.Types
import System.Random

getSuperstitiousMove :: ArchetypeConfig 'Superstitious -> GameContext -> State StdGen Move
getSuperstitiousMove config ctx =
    case findApplicableSuperstition (scBeliefs config) ctx of
        Just forcedMove -> pure forcedMove
        Nothing -> pure $ lookupFallbackStrategy (scFallbackChart config) ctx

findApplicableSuperstition :: [Superstition] -> GameContext -> Maybe Move
findApplicableSuperstition beliefs ctx =
    let hand = _contextPlayerHand ctx
        dealerCard = _contextDealerUpcard ctx
     in case filter (superstitionApplies hand dealerCard) beliefs of
            (belief : _) -> Just (superstitionMove belief)
            [] -> Nothing

superstitionApplies :: SomeHand -> Card -> Superstition -> Bool
superstitionApplies hand (Card dealerRank _) = \case
    NeverHitOn16 -> handScore hand == 16
    AlwaysStandSoft18 -> handScore hand == 18 && isSoftHand hand
    NeverSplitTens -> case extractPairRank hand of
        Just Ten -> True
        _ -> False
    AlwaysSplitAces -> case extractPairRank hand of
        Just Ace -> True
        _ -> False
    NeverDoubleDown -> False
    StandOn12VsDealer2Or3 ->
        handScore hand == 12 && (dealerRank == Two || dealerRank == Three)

superstitionMove :: Superstition -> Move
superstitionMove = \case
    NeverHitOn16 -> Stand
    AlwaysStandSoft18 -> Stand
    NeverSplitTens -> Hit
    AlwaysSplitAces -> Split
    NeverDoubleDown -> Hit
    StandOn12VsDealer2Or3 -> Stand

lookupFallbackStrategy :: StrategyChart -> GameContext -> Move
lookupFallbackStrategy chart ctx =
    let hand = _contextPlayerHand ctx
        upcard = rank (_contextDealerUpcard ctx)
        offering = _contextOffering ctx
        decision = safeDecisionLookup chart hand upcard offering
     in decisionToMove decision

decisionToMove :: Decision -> Move
decisionToMove (Always move) = move
decisionToMove (Prefer primary (Else _)) = primary

isSoftHand :: SomeHand -> Bool
isSoftHand _ = False
