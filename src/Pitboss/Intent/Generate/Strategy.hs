module Pitboss.Intent.Generate.Strategy where

import Control.Lens
import Pitboss.Blackjack.Materia.Card
import Pitboss.Intent.Context
import Pitboss.Strategy.Chart.Interpret
import Pitboss.Strategy.Chart.Types
import Pitboss.Strategy.Types

lookupBasicStrategy :: GameContext -> Move
lookupBasicStrategy ctx =
    let chart = getBasicStrategyChart
        hand = ctx ^. contextPlayerHand
        upcard = rank (ctx ^. contextDealerUpcard)
        offering = ctx ^. contextOffering
        decision = safeDecisionLookup chart hand upcard offering
     in decisionToMove decision

lookupPerfectStrategy :: GameContext -> Move
lookupPerfectStrategy = lookupBasicStrategy

decisionToMove :: Decision -> Move
decisionToMove (Always move) = move
decisionToMove (Prefer primary (Else _)) = primary

getBasicStrategyChart :: StrategyChart
getBasicStrategyChart = []
