module Pitboss.Sim.State.SpotHand.Mutation where

import Control.Lens hiding (universe)
import Pitboss.Blackjack.FSM.Hand (HandFSM (..), HandTerminus (..), SomeHandFSM (..))
import Pitboss.Sim.State.Spot (SpotHandIx, SpotState (..))
import Pitboss.Sim.State.Spot.Lens (lensHands)
import Pitboss.Sim.State.SpotHand (SpotHandState (..))
import Pitboss.Sim.State.SpotHand.Lens (lensSpotHandFSM)
import Pitboss.Sim.Types.FiniteMap (insertFiniteMap, lookupFiniteMap)
import Pitboss.Sim.Types.Occupancy (Occupancy (..))
import Pitboss.Types.BoundedEnum (universe)

freezeOnly :: SpotHandIx -> SpotHandIx -> Occupancy SpotHandState -> Occupancy SpotHandState
freezeOnly target current (Present s)
  | target == current = Present s {spotHandFSM = toCompleted s}
  | otherwise = Present s
  where
    toCompleted s' = case s' ^. lensSpotHandFSM of
      SomeHandFSM _ -> SomeHandFSM (CompletedFSM Stand)
freezeOnly _ _ occ = occ

freezeSelectedPlayHand :: SpotHandIx -> SpotState -> SpotState
freezeSelectedPlayHand target spot =
  spot & lensHands %~ mapWithKeyFreeze
  where
    mapWithKeyFreeze =
      foldr
        ( \k f ->
            case lookupFiniteMap k (hands spot) of
              Just occ -> insertFiniteMap k (freezeOnly target k occ) . f
              Nothing -> f
        )
        id
        universe
