{- | Public facade for all Pitboss simulation modules.

This module re-exports the simulation components needed for running
blackjack games with various agent strategies. For most use cases,
importing this single module provides everything needed for simulation.
-}
module Pitboss.Simulation (
    -- * Core Types
    module Pitboss.Simulation.Types,
    module Pitboss.Simulation.Event,

    -- * Intent System
    module Pitboss.Simulation.Intent.Types,
    module Pitboss.Simulation.Intent.Generate,
    module Pitboss.Simulation.Intent.Validate,

    -- * Agent System
    module Pitboss.Simulation.Agents.Types,

    -- * Player Agents
    module Pitboss.Simulation.Agents.Player.Basic,
    module Pitboss.Simulation.Agents.Player.Perfect,
    module Pitboss.Simulation.Agents.Player.Advantage,
    module Pitboss.Simulation.Agents.Player.Superstitious,

    -- * Dealer Agents
    module Pitboss.Simulation.Agents.Dealer,

    -- * Simulation Engine
    module Pitboss.Simulation.Engine.DeltaGen,
    module Pitboss.Simulation.Engine.Runtime,
) where

-- Core types

import Pitboss.Simulation.Event
import Pitboss.Simulation.Types

-- Intent system

import Pitboss.Simulation.Intent.Generate
import Pitboss.Simulation.Intent.Types
import Pitboss.Simulation.Intent.Validate

-- Agent system
import Pitboss.Simulation.Agents.Types

-- Player agents

import Pitboss.Simulation.Agents.Player.Advantage
import Pitboss.Simulation.Agents.Player.Basic
import Pitboss.Simulation.Agents.Player.Perfect
import Pitboss.Simulation.Agents.Player.Superstitious

-- Dealer agents
import Pitboss.Simulation.Agents.Dealer

-- Simulation engine
import Pitboss.Simulation.Engine.DeltaGen
import Pitboss.Simulation.Engine.Runtime
