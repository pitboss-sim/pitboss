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

    -- | Only the most common deck operations are re-exported.
    -- Import "Pitboss.Blackjack.Materia.Deck" for full API.
    Deck,
    mkDeck,
    drawCard,
    shuffle,
    fullDeck,

    -- * Game Rules and Offerings
    module Pitboss.Blackjack.Rules,

    -- * Well-known Configurations
    vegas6,
    downtownSingleDeck,

    -- * Actions and Events
    module Pitboss.Blackjack.Actions,

    -- * Game Outcomes
    module Pitboss.Blackjack.Outcomes,

    -- * Game Logic
    module Pitboss.Blackjack.Play,
) where

import Pitboss.Blackjack.Types
import Pitboss.Blackjack.Types.Core

import Pitboss.Blackjack.Deck (Deck, drawCard, fullDeck, mkDeck, shuffle)

import Pitboss.Blackjack.Rules

import Pitboss.Blackjack.WellKnown (downtownSingleDeck, vegas6)

import Pitboss.Blackjack.Actions
import Pitboss.Blackjack.Outcomes
import Pitboss.Blackjack.Play
