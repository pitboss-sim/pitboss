{-# LANGUAGE TupleSections #-}

module Pitboss.Strategy.Chart.Overlay
  ( reduceOverlay,
  )
where

import Control.Category ((>>>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Pitboss.Blackjack.Card (Rank)
import Pitboss.Strategy.Chart.Parse (ChartEntry (..))
import Pitboss.Strategy.Chart.Types (HandPrefix, MoveCode)

-- Index chart entries by (HandPrefix, Rank)
indexEntries :: [ChartEntry] -> Map (HandPrefix, Rank) MoveCode
indexEntries =
  Map.unions . map entryToMap
  where
    entryToMap (ChartEntry hp moves') =
      Map.mapKeys (hp,) moves'

-- Overlay your custom chart over the baseline
overlayStrategy :: [ChartEntry] -> [ChartEntry] -> [ChartEntry]
overlayStrategy baseline custom =
  let baseMap = indexEntries baseline
      customMap = indexEntries custom
      combined = Map.union customMap baseMap
   in groupBackToChart combined

-- Group back by HandPrefix -> [ChartEntry]
groupBackToChart :: Map (HandPrefix, Rank) MoveCode -> [ChartEntry]
groupBackToChart =
  Map.toList
    >>> foldr insert Map.empty
    >>> Map.toList
    >>> map (uncurry ChartEntry)
  where
    insert ((hp, up), mv) = Map.insertWith Map.union hp (Map.singleton up mv)

-- Reduce a list of charts into one, using overlay semantics.
-- Later entries override earlier ones.
reduceOverlay :: [[ChartEntry]] -> [ChartEntry]
reduceOverlay = foldr overlayStrategy []
