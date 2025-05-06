module Pitboss.Sim.State.Table.Mutation where

import Pitboss.Sim.State.Spot (SpotState, initSpot)
import Pitboss.Sim.State.Table (TableSpotIx, TableState (..))
import Pitboss.Sim.Types.FiniteMap (FiniteMap, insertFiniteMap, lookupFiniteMap)
import Pitboss.Sim.Types.Occupancy (Occupancy (..))
import Pitboss.Sim.World.Identifier (ActorId)

updateSpot :: TableSpotIx -> (SpotState -> SpotState) -> FiniteMap TableSpotIx (Occupancy SpotState) -> FiniteMap TableSpotIx (Occupancy SpotState)
updateSpot sid f spots =
  insertFiniteMap sid updatedSpot spots
  where
    updatedSpot = case lookupFiniteMap sid spots of
      Just (Present spot) -> Present (f spot)
      Just Absent -> error "Cannot update absent spot"
      Nothing -> error "Spot does not exist"

claimSpot :: TableSpotIx -> ActorId -> TableState fsm -> Either String (TableState fsm)
claimSpot spotId actorId ts =
  case lookupFiniteMap spotId (playerSpots ts) of
    Just Absent ->
      let updatedSpots = insertFiniteMap spotId (Present (initSpot actorId)) (playerSpots ts)
       in Right ts {playerSpots = updatedSpots}
    Just (Present _) ->
      Left $ "Spot " ++ show spotId ++ " is already claimed."
    Nothing ->
      Left $ "Invalid spot: " ++ show spotId

yieldSpot :: TableSpotIx -> TableState fsm -> Either String (TableState fsm)
yieldSpot spotId ts =
  case lookupFiniteMap spotId (playerSpots ts) of
    Just (Present _) ->
      let updatedSpots = insertFiniteMap spotId Absent (playerSpots ts)
       in Right ts {playerSpots = updatedSpots}
    Just Absent ->
      Left $ "Spot " ++ show spotId ++ " is already unclaimed."
    Nothing ->
      Left $ "Invalid spot: " ++ show spotId
