module Pitboss.Strategy.Chart.Validate where

import Data.Map.Strict (toList)
import Pitboss.Blackjack.Card (Rank)
import Pitboss.Strategy.Chart.Types (ChartEntry (..), HandPrefix, MoveCode (..))

validateStrategyChart :: [ChartEntry] -> [(HandPrefix, Rank)]
validateStrategyChart =
  concatMap validateEntry
  where
    validateEntry (ChartEntry hp moves') =
      [ (hp, up)
        | (up, mv) <- toList moves',
          mv == MoveUndefined
      ]
