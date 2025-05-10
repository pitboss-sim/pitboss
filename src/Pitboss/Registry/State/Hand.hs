module Pitboss.Registry.State.Hand where

import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Registry.State.Types.Clocked (Clocked (..), Tick)
import Pitboss.Registry.State.Types.DeltaDriven (DeltaDriven (..))
import Pitboss.Registry.State.Types.Snapshot (StateSnapshot (..))

data HandState = HandState
  { handCards :: [Card],
    originalBet :: Chips,
    splitDepth :: Int,
    handIx :: Int
  }
  deriving (Eq, Show)

applySnapshotDelta ::
  (DeltaDriven entity delta, Clocked entity) =>
  -- | Tick update function (e.g., (+1))
  (Tick -> Tick) ->
  delta ->
  StateSnapshot entity delta ->
  StateSnapshot entity delta
applySnapshotDelta bumpTick delta (StateSnapshot entity history) =
  let oldTick = tick entity
      newTick = bumpTick oldTick
      updatedEntity = applyDelta delta entity
      updatedHistory = IHM.insert newTick delta history
   in StateSnapshot
        { entity = setTick newTick updatedEntity,
          history = updatedHistory
        }
