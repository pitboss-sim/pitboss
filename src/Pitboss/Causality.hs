{- | Public facade for all Pitboss state management modules.

This module re-exports all state-related modules for convenient access.
For most use cases, importing this single module provides everything needed
for working with the Pitboss state system.
-}
module Pitboss.Causality (
    -- * Core Types
    module Pitboss.Causality.Types.Core,
    module Pitboss.Causality.Types.FiniteMap,
    module Pitboss.Causality.Types.FiniteMap.BoundedEnum,
    module Pitboss.Causality.Types.FiniteMap.Occupancy,

    -- * Entity System
    module Pitboss.Causality.Entity.Types,
    module Pitboss.Causality.Entity.Lenses,

    -- * Delta System
    module Pitboss.Causality.Delta.Types,

    -- * Timeline & Registry
    module Pitboss.Causality.Timeline,
    module Pitboss.Causality.Timeline.Query,
    module Pitboss.Causality.Timeline.Reconstruction,
    module Pitboss.Causality.Registry,

    -- * Trace System
    module Pitboss.Causality.Trace,
    module Pitboss.Causality.Trace.Types,
    module Pitboss.Causality.Trace.Ops,

    -- * Runtime State
    module Pitboss.Causality.TickCache,
) where

-- Core types
import Pitboss.Causality.Types.Core
import Pitboss.Causality.Types.FiniteMap
import Pitboss.Causality.Types.FiniteMap.BoundedEnum
import Pitboss.Causality.Types.FiniteMap.Occupancy

-- Entity system

import Pitboss.Causality.Entity.Lenses
import Pitboss.Causality.Entity.Types

-- Delta system
import Pitboss.Causality.Delta.Types

-- Timeline & Registry

import Pitboss.Causality.Registry
import Pitboss.Causality.Timeline
import Pitboss.Causality.Timeline.Query
import Pitboss.Causality.Timeline.Reconstruction

-- Trace system
import Pitboss.Causality.Trace
import Pitboss.Causality.Trace.Ops
import Pitboss.Causality.Trace.Types

-- Runtime state
import Pitboss.Causality.TickCache
