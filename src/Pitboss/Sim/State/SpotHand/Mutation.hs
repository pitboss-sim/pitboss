module Pitboss.Sim.State.SpotHand.Mutation where

import Control.Lens
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Hand (Hand (..))
import Pitboss.Sim.State.Spot
import Pitboss.Sim.State.Spot.Lens (lensHands)
import Pitboss.Sim.State.SpotHand (SpotHandPlayState (..), SpotHandState (..))
import Pitboss.Sim.Types.FiniteMap
import Pitboss.Sim.Types.Occupancy (Occupancy (..), isAbsent)

freezeOnly :: SpotHandIx -> SpotHandIx -> Occupancy SpotHandState -> Occupancy SpotHandState
freezeOnly target current (Present (SpotHandState NormalPlay h))
  | target == current = Present (SpotHandState HandFrozen h)
freezeOnly _ _ occ = occ

freezeSelectedPlayHand :: SpotHandIx -> SpotState -> SpotState
freezeSelectedPlayHand target spot =
  spot & lensHands %~ mapFiniteMapWithKey (freezeOnly target)

nextFreeHands :: SpotState -> [SpotHandIx]
nextFreeHands spot =
  [hid | (hid, occ) <- toListFiniteMap (spot ^. lensHands), isAbsent occ]

addCardsToSplitPlayHands ::
  Card -> Card -> Card -> Card -> SpotHandIx -> SpotHandIx -> SpotState -> SpotState
addCardsToSplitPlayHands orig1 new1 orig2 new2 h1 h2 =
  over lensHands $
    insertFiniteMap h2 (Present (SpotHandState NormalPlay (Hand [orig2, new2])))
      . insertFiniteMap h1 (Present (SpotHandState NormalPlay (Hand [orig1, new1])))

nextAvailableSpotHand :: SpotState -> Maybe SpotHandIx
nextAvailableSpotHand spot =
  let isLive (_, Present (SpotHandState ps _)) = ps == NormalPlay
      isLive _ = False
   in fmap fst . safeHead . filter isLive . toListFiniteMap $ spot ^. lensHands

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x
