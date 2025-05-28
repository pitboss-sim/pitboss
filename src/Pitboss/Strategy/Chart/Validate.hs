module Pitboss.Strategy.Chart.Validate where

import Data.Map.Strict (toList)
import Pitboss.Blackjack.Materia.Card (Rank)
import Pitboss.Blackjack.Materia.Hand (HandKind)
import Pitboss.Strategy.Chart.Types (ChartEntry (..), MoveCode (..), kindToHandPrefix)

validateStrategyChart :: [ChartEntry] -> [(HandKind, Maybe Int, Rank)]
validateStrategyChart =
    concatMap validateEntry
  where
    validateEntry (ChartEntry kind value moves') =
        [ (kind, value, up)
        | (up, mv) <- toList moves'
        , mv == MoveUndefined
        ]

prettyValidationError :: (HandKind, Maybe Int, Rank) -> String
prettyValidationError (kind, value, up) =
    let prefix = kindToHandPrefix kind value
     in "  Undefined move for hand: " ++ show prefix ++ ", dealer upcard: " ++ show up
