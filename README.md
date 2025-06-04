# Pitboss

A blackjack system written in Haskell, designed for both realistic gameplay and large-scale simulation.

## Overview

*Note: Pitboss is in active development - see Status section for current implementation state.*

Pitboss is a dual-purpose blackjack system that aims to serve both as a realistic game implementation and a simulation framework. The project takes an unconventional approach by modeling blackjack as multiplexed 1v1 "bouts" between dealer and player hands, emphasizing correctness through careful domain modeling.

A "bout" represents the fundamental interaction in blackjack: a single player hand competing against the dealer's hand. While blackjack appears to be one dealer versus multiple players, it's actually several independent competitions happening in parallel. This bout-centric modeling simplifies simulation logic, enables more efficient parallelization, and makes it easier to analyze individual hand outcomes without the complexity of table-level interactions.

The goal is to support both playable blackjack games with authentic casino rules and large-scale Monte Carlo simulations for strategy analysis. The project focuses specifically on winnable, countable blackjack variants where skilled play can achieve positive expected value.

### Key Features

**Domain-Driven Design**
- Bout-centric modeling that captures the true nature of blackjack interactions
- State machines with compile-time guarantees for valid game transitions
- Preservation of casino terminology and mental models

**Event Sourcing Architecture** (Work in Progress)
- Intent → event → delta → trace pipeline for complete game history
- Entity-component system with attrs/modes/rels decomposition
- Timeline reconstruction capabilities (under development)
- Foundation for causal analysis of player decisions and outcomes

**Gameplay and Simulation Support** (Partially Implemented)
- Multiple rule variations planned (Vegas 6-deck, Downtown single-deck)
- Basic Strategy implementation with chart parsing
- Player archetype framework (BasicStrategy implemented, others planned)
- Core game rules including Peek/ENHC, H17/S17, DAS
- Focus on games with favorable rules and penetration for card counting analysis

**What Sets Pitboss Apart**
- **Time-travel debugging**: The event sourcing architecture allows stepping through any game backwards and forwards, inspecting state at any point
- **Casino ecosystem modeling**: Not just game rules, but table limits, penetration, burn policies, mid-shoe entry, and other house policies that affect real play
- **Realistic player populations**: Model tables with mixed skill levels—basic strategy players, card counters, and superstitious players making predictable mistakes
- **Chart-based strategy system**: Load and overlay strategy modifications from text files, making it easy to test variations and house-specific adjustments
- **Built for scale**: Architecture designed from the ground up to support millions of hands for Monte Carlo analysis (implementation in progress)

### Motivation

This project emerged from a desire to build a blackjack system that truly respects the domain. Rather than treating blackjack as a simple card game, Pitboss aims to model the full complexity of casino blackjack including:

- The distinction between a "hand" as cards held versus a hand as a playable position
- The temporal nature of game decisions and dealer procedures
- The relationship between game rules, table rules, and house policies
- The ability to analyze not just outcomes but the full decision tree
- Hand lifecycle modeling that captures the progression from initial deal through all possible actions

The goal is to build a system where simulations reflect actual gameplay mechanics. The architecture makes invalid game states unrepresentable and provides compile-time guarantees about game flow correctness.

A longer-term aspiration is to enable causal analysis of blackjack decisions—understanding not just what happened, but why specific choices led to particular outcomes across different game contexts.

## License

This project is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0) only.

## Status & Current State

Pitboss is under active development. The core domain model and basic game flow are in place, with the event sourcing pipeline and simulation framework as works in progress.

### Implemented
- Core game flow for hit/stand decisions
- Basic strategy with chart parsing and overlay system
- Event sourcing foundation (intent → event → delta → trace)
- Bout-based domain model with type-safe FSMs
- Peek/ENHC and H17/S17 rule variations

### In Progress
- Split, surrender, and insurance mechanics
- Timeline reconstruction and "time-travel" debugging
- Additional player archetypes (Perfect, Advantage, Superstitious)
- Comprehensive test coverage

### Planned
- Performance optimization for million-hand simulations
- Parallel bout processing
- Analysis and debugging tools
- Production-ready error handling
