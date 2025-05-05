{-# LANGUAGE FlexibleContexts #-}

module Pitboss.Sim.State.Spot.Mutation where

import Control.Lens hiding (universe)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Hand (Hand (..))
import Pitboss.Sim.State.Spot (SpotHandIx, SpotState (..), Turn (..))
import Pitboss.Sim.State.Spot.Lens (lensHands)
import Pitboss.Sim.State.SpotHand (SpotHandPlayState (..), SpotHandState (..))
import Pitboss.Sim.Types.FiniteMap (insertFiniteMap, lookupFiniteMap, toListFiniteMap)
import Pitboss.Sim.Types.Occupancy (Occupancy (..), isAbsent)
import Pitboss.Types.BoundedEnum (universe)

advanceToNextHand :: SpotState -> SpotState
advanceToNextHand spot@(SpotState handsMap currentTurn _) =
  let activeHands = [hid | hid <- universe, lookupFiniteMap hid handsMap /= Just Absent]
   in case currentTurn of
        NoHandSelected ->
          case activeHands of
            (hid : _) -> spot {turn = Playing hid}
            [] -> spot {turn = NoHandSelected}
        Playing current ->
          let remaining = dropWhile (/= current) activeHands
           in case remaining of
                (_ : next : _) -> spot {turn = Playing next}
                _ -> spot {turn = NoHandSelected}

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
