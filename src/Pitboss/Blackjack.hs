{- | Facade module for blackjack domain types and operations.

This module re-exports the most commonly used types and functions
for working with blackjack game logic. For specialized needs,
you can still import the specific sub-modules directly.
-}
module Pitboss.Blackjack (
    -- * Cards and Hands
    module Pitboss.Blackjack.Types,
    module Pitboss.Blackjack.Types.Core,

    -- * Deck Operations
    module Pitboss.Blackjack.Deck,

    -- * Game Rules and Offerings
    module Pitboss.Blackjack.Rules,

    -- * Well-known Configurations
    module Pitboss.Blackjack.WellKnown,

    -- * Actions and Events
    module Pitboss.Blackjack.Actions,

    -- * Game Outcomes
    module Pitboss.Blackjack.Outcomes,

    -- * Game Logic
    module Pitboss.Blackjack.Play,
) where

import Pitboss.Blackjack.Types
import Pitboss.Blackjack.Types.Core
import Pitboss.Blackjack.Deck
import Pitboss.Blackjack.Rules
import Pitboss.Blackjack.WellKnown
import Pitboss.Blackjack.Actions
import Pitboss.Blackjack.Outcomes
import Pitboss.Blackjack.Play
