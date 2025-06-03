# Pitboss

A blackjack simulation system written in Haskell, emphasizing domain modeling and correctness.

## Overview

Mastering blackjack is notoriously difficult—the interplay of card counting, basic strategy deviations, betting systems, and bankroll management creates a complex decision space that even experienced players struggle to navigate optimally. Pitboss and future products derived from it hope to make understanding the underlying mathematics more accessible.

Put simply, **Pitboss is a blackjack simulation system** that models the full complexity of casino play through careful domain modeling. The core simulation can model a player sitting down at a table, making strategic decisions, and playing complete hands against the dealer—all captured through a sophisticated event sourcing architecture. It aims to enable deep causal analysis of player decisions, "what if" scenarios, [Monte Carlo](https://en.wikipedia.org/wiki/Monte_Carlo_method) analytics, full game replay and time-travel debugging, and massive-scale simulation for EV and house edge calculations.

The project takes an unconventional approach by modeling blackjack as multiplexed 1v1 "bouts" between dealer and players. While blackjack appears to be one dealer versus multiple players, it's actually several independent competitions happening in parallel. This bout-centric modeling simplifies simulation logic, enables efficient parallelization, and makes it easier to analyze individual hand outcomes without table-level complexity.

The focus is on winnable, countable blackjack variants where skilled play can achieve positive expected value—the games worth studying and mastering.

### What Makes Pitboss Different

- **Time-travel debugging**: Step through any game backwards and forwards, inspecting state at any decision point
- **Bout-centric modeling**: Models blackjack as it really is—independent 1v1 competitions, not one dealer vs many players
- **Complete audit trail**: Event sourcing captures every decision, enabling deep causal analysis of why strategies succeed or fail
- **Casino ecosystem modeling**: Table limits, penetration, burn policies, and house rules that affect real play
- **Chart-based strategy system**: Load and test strategy variations from text files
- **Compile-time correctness**: Type-safe state machines make invalid game states impossible

### Why Pitboss?

Most blackjack simulators treat the game as a simple card-matching exercise. Pitboss respects the full complexity of casino play—the distinction between cards held and playable positions, the temporal nature of dealer procedures, and the intricate relationships between game rules, table policies, and house edge.

The goal is causal analysis: not just *what* happened, but *why* specific choices led to particular outcomes across different contexts. When you can step through every decision backwards and forwards, patterns emerge that static analysis misses.

## License

This project is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0) only.

## Status

**Core blackjack simulation is working.** You can run a complete player vs dealer bout with hit/stand/double decisions, basic strategy, and proper outcomes. The event sourcing pipeline captures everything for replay and analysis.

**What works today:**
- Single-bout gameplay with all basic moves
- Chart-based basic strategy player
- Event sourcing with time-travel debugging
- H17/S17 and other rule variations
- Complete hand scoring and dealer logic

**In progress:**
- Split, insurance, and surrender mechanics (architecture done, economic integration needed)
- Multi-bout table coordination
- Advanced player agents (card counting, etc.)

**Next up:** Complete the remaining game mechanics and build the orchestrator for automated gameplay.
