{-# LANGUAGE RankNTypes #-}

module Pitboss.Sim.State.Table.Lens where

import Control.Lens (Lens', lens)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Sim.State.Shoe (ShoeState)
import Pitboss.Sim.State.Spot (SpotState)
import Pitboss.Sim.State.Table
  ( SomeTableState (..),
    TableSpotIx,
    TableState (..),
  )
import Pitboss.Sim.Types.FiniteMap (FiniteMap, insertFiniteMap, lookupFiniteMap)
import Pitboss.Sim.Types.Occupancy (Occupancy)
import Pitboss.Sim.World.Identifier (ActorId)

lensDeck :: Lens' (TableState fsm) [Card]
lensDeck = lens deck (\s x -> s {deck = x})

lensShoeState :: Lens' (TableState fsm) ShoeState
lensShoeState = lens shoeState (\s x -> s {shoeState = x})

lensPlayerSpots :: Lens' (TableState fsm) (FiniteMap TableSpotIx (Occupancy SpotState))
lensPlayerSpots = lens playerSpots (\s x -> s {playerSpots = x})

lensTableDealerId :: Lens' (TableState fsm) (Maybe ActorId)
lensTableDealerId = lens tableDealerId (\s x -> s {tableDealerId = x})

lensFiniteMapAt :: (Ord k) => k -> Lens' (FiniteMap k v) (Maybe v)
lensFiniteMapAt k =
  lens
    (lookupFiniteMap k)
    ( \m mv ->
        case mv of
          Just v -> insertFiniteMap k v m
          Nothing -> m
    )

-- operate on the TableState inside SomeTableState (if matching)
withSomeTableState ::
  (forall fsm. TableState fsm -> TableState fsm) ->
  SomeTableState ->
  SomeTableState
withSomeTableState f (SomeTableState ts) = SomeTableState (f ts)
