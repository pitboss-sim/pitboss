-- | Public facade for all Pitboss state management modules.
--
-- This module re-exports all state-related modules for convenient access.
-- For most use cases, importing this single module provides everything needed
-- for working with the Pitboss state system.
module Pitboss.State
  ( -- * Core Types
    module Pitboss.State.Types.Core
  , module Pitboss.State.Types.FiniteMap
  , module Pitboss.State.Types.FiniteMap.BoundedEnum
  , module Pitboss.State.Types.FiniteMap.Occupancy

    -- * Entity System
  , module Pitboss.State.Entity.Types
  , module Pitboss.State.Entity.Lenses

    -- * Delta System
  , module Pitboss.State.Delta.Types

    -- * Timeline & Registry
  , module Pitboss.State.Timeline
  , module Pitboss.State.Timeline.Query
  , module Pitboss.State.Timeline.Reconstruction
  , module Pitboss.State.Registry

    -- * Trace System
  , module Pitboss.State.Trace
  , module Pitboss.State.Trace.Types
  , module Pitboss.State.Trace.Ops

    -- * Runtime State
  , module Pitboss.State.TickCache
  ) where

-- Core types
import Pitboss.State.Types.Core
import Pitboss.State.Types.FiniteMap
import Pitboss.State.Types.FiniteMap.BoundedEnum
import Pitboss.State.Types.FiniteMap.Occupancy

-- Entity system
import Pitboss.State.Entity.Types
import Pitboss.State.Entity.Lenses

-- Delta system
import Pitboss.State.Delta.Types

-- Timeline & Registry
import Pitboss.State.Timeline
import Pitboss.State.Timeline.Query
import Pitboss.State.Timeline.Reconstruction
import Pitboss.State.Registry

-- Trace system
import Pitboss.State.Trace
import Pitboss.State.Trace.Types
import Pitboss.State.Trace.Ops

-- Runtime state
import Pitboss.State.TickCache
