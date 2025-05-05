module Pitboss.Sim.State.Table.Mutation where

import Pitboss.Sim.State.Spot (SpotState)
import Pitboss.Sim.State.Table (TableSpotIx)
import Pitboss.Sim.Types.FiniteMap (FiniteMap, insertFiniteMap, lookupFiniteMap)
import Pitboss.Sim.Types.Occupancy (Occupancy (..))

updateSpot :: TableSpotIx -> (SpotState -> SpotState) -> FiniteMap TableSpotIx (Occupancy SpotState) -> FiniteMap TableSpotIx (Occupancy SpotState)
updateSpot sid f spots =
  insertFiniteMap sid updatedSpot spots
  where
    updatedSpot = case lookupFiniteMap sid spots of
      Just (Present spot) -> Present (f spot)
      Just Absent -> error "Cannot update absent spot"
      Nothing -> error "Spot does not exist"
