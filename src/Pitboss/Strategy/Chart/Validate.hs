module Pitboss.Strategy.Chart.Validate
  ( validateStrategyChart,
  )
where

import Data.Map.Strict (toList)
import Pitboss.Blackjack.Card (Rank)
import Pitboss.Strategy.Chart.Parse (ChartEntry (..), MoveCode (..))
import Pitboss.Strategy.Chart.Types (HandPrefix)

validateStrategyChart :: [ChartEntry] -> [(HandPrefix, Rank)]
validateStrategyChart =
  concatMap validateEntry
  where
    validateEntry (ChartEntry hp moves') =
      [ (hp, up)
        | (up, mv) <- toList moves',
          mv == MoveUndefined
      ]
