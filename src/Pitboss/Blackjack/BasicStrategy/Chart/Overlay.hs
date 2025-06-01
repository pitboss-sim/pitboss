{-# LANGUAGE TupleSections #-}

module Pitboss.Blackjack.BasicStrategy.Chart.Overlay where

import Control.Category ((>>>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Pitboss.Blackjack.BasicStrategy.Chart.Types (ChartEntry (..), MoveCode)
import Pitboss.Blackjack.Materia.Card (Rank)
import Pitboss.Blackjack.Materia.Hand (HandKind)

-- Use the actual chart entry structure for indexing
indexEntries :: [ChartEntry] -> Map (HandKind, Maybe Int, Rank) MoveCode
indexEntries =
    Map.unions . map entryToMap
  where
    entryToMap (ChartEntry kind value moves') =
        Map.mapKeys (kind,value,) moves'

overlayStrategy :: [ChartEntry] -> [ChartEntry] -> [ChartEntry]
overlayStrategy baseline custom =
    let baseMap = indexEntries baseline
        customMap = indexEntries custom
        combined = Map.union customMap baseMap
     in groupBackToChart combined

groupBackToChart :: Map (HandKind, Maybe Int, Rank) MoveCode -> [ChartEntry]
groupBackToChart =
    Map.toList
        >>> foldr insert Map.empty
        >>> Map.toList
        >>> map (\((kind, value), moves') -> ChartEntry kind value moves')
  where
    insert ((kind, value, up), mv) =
        Map.insertWith Map.union (kind, value) (Map.singleton up mv)

-- Reduce a list of charts into one, using overlay semantics.
-- Later entries override earlier ones.
reduceOverlay :: [[ChartEntry]] -> [ChartEntry]
reduceOverlay = foldr overlayStrategy []
