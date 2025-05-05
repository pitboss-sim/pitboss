module Pitboss.Sim.State.Table.Lens where

import Control.Lens (Lens', lens)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Sim.State.Spot (SpotState)
import Pitboss.Sim.State.Table (ShoeState, TableSpotIx, TableState (..))
import Pitboss.Sim.Types.FiniteMap (FiniteMap, insertFiniteMap, lookupFiniteMap)
import Pitboss.Sim.Types.Occupancy (Occupancy)

lensGameFSM :: Lens' (TableState fsm) fsm
lensGameFSM = lens gameFSM (\s x -> s {gameFSM = x})

lensDeck :: Lens' (TableState fsm) [Card]
lensDeck = lens deck (\s x -> s {deck = x})

lensShoeState :: Lens' (TableState fsm) ShoeState
lensShoeState = lens shoeState (\s x -> s {shoeState = x})

lensPlayerSpots :: Lens' (TableState fsm) (FiniteMap TableSpotIx (Occupancy SpotState))
lensPlayerSpots = lens playerSpots (\s x -> s {playerSpots = x})

lensDealerSpot :: Lens' (TableState fsm) SpotState
lensDealerSpot = lens dealerSpot (\s x -> s {dealerSpot = x})

lensFiniteMapAt :: (Ord k) => k -> Lens' (FiniteMap k v) (Maybe v)
lensFiniteMapAt k =
  lens
    (lookupFiniteMap k)
    ( \m mv ->
        case mv of
          Just v -> insertFiniteMap k v m
          Nothing -> m
    )
