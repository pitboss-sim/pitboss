{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pitboss.Sim.World.Relations where

import Control.Lens ((^.))
import Pitboss.Sim.State.Actor (ActorState)
import Pitboss.Sim.State.Round
import Pitboss.Sim.State.Shoe (ShoeState)
import Pitboss.Sim.State.Spot (SpotState (..))
import Pitboss.Sim.State.SpotHand (SpotHandState (..))
import Pitboss.Sim.State.Table
  ( SomeTableState,
    TableState (..),
  )
import Pitboss.Sim.World
import Pitboss.Sim.World.Identifier
import Pitboss.Sim.World.Registry (lookupEntity)

-- 🌍 World Entity Hierarchy
--
-- Top-down containment and identifier relationships in the simulation:
--
-- World
-- └── TableId → TableState
--     ├── Offering               -- Static game rules and configuration
--     ├── ShoeId                 -- The active shoe (deck)
--     ├── RoundId                -- The current round of play
--     ├── SpotId (≤6)            -- Player spots at the table
--     │   └── SpotHandId (≤4)    -- A player's hands (original + splits)
--
-- Additional associations:
-- - Table references an ActorId (dealer)
-- - Spot references an ActorId (player)
-- - All entities are stored in registries keyed by their respective *Id
-- - Dereferencing between these layers is handled by the `Deref` typeclass
--
-- Example: resolve a SpotHand to its Table
--     derefChain w spotHandId >>= derefChain w >>= deref w

-- | Deref allows resolution of an entity id into a state model within a WorldState
class Deref id entity where
  deref :: WorldState -> id -> Maybe entity

instance Deref ActorId ActorState where
  deref w aId = lookupEntity aId (_actors w)

instance Deref ShoeId ShoeState where
  deref w sId = lookupEntity sId (_shoes w)

instance Deref RoundId RoundState where
  deref w rId = lookupEntity rId (_rounds w)

instance Deref SpotId SpotState where
  deref w sId = lookupEntity sId (_spots w)

instance Deref SpotHandId SpotHandState where
  deref w shId = lookupEntity shId (_spotHands w)

instance Deref TableId SomeTableState where
  deref w tid = lookupEntity tid (w ^. lensTables)

--- second-order dereferencing

instance Deref SpotId ActorState where
  deref w sid = do
    spot <- deref w sid
    deref w (spotActorId spot)

instance Deref TableId ActorState where
  deref w tid = do
    table <- deref w tid
    deref w (tableDealerId table)

instance Deref SpotHandId SpotState where
  deref w hid = do
    hand <- deref w hid
    deref w (spotId hand)

instance Deref SpotHandId ActorState where
  deref w hid = do
    hand <- deref w hid
    deref w (spotId hand)

instance Deref TableId ShoeState where
  deref w tid = do
    table <- deref w tid
    deref w (tableShoeId table)

instance Deref RoundId ShoeState where
  deref w rid = do
    round' <- deref w rid
    deref w (roundShoeId round')

-- Resolve one ID to another using a dereferencing function

-- From a SpotId, get the ActorId (player)
derefSpotActorId :: WorldState -> SpotId -> Maybe ActorId
derefSpotActorId w sid = spotActorId <$> deref w sid

derefTableDealerId :: WorldState -> TableId -> Maybe ActorId
derefTableDealerId w tid = tableDealerId <$> deref w tid

derefSpotHandSpotId :: WorldState -> SpotHandId -> Maybe SpotId
derefSpotHandSpotId w shid = spotId <$> deref w shid

derefSpotHandActorId :: WorldState -> SpotHandId -> Maybe ActorId
derefSpotHandActorId w shid = do
  spot <- deref w shid >>= deref w . spotId
  pure (spotActorId spot)

derefTableShoeId :: WorldState -> TableId -> Maybe ShoeId
derefTableShoeId w tid = tableShoeId <$> deref w tid

derefTableRoundId :: WorldState -> TableId -> Maybe RoundId
derefTableRoundId w tid = tableRoundId <$> deref w tid

derefRoundShoeId :: WorldState -> RoundId -> Maybe ShoeId
derefRoundShoeId w rid = roundShoeId <$> deref w rid
