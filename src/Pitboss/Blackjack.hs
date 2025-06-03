{- | Facade module for blackjack domain types and operations.

This module re-exports the most commonly used types and functions
for working with blackjack game logic. For specialized needs,
you can still import the specific sub-modules directly.
-}
module Pitboss.Blackjack (
    -- * Cards and Hands
    module Pitboss.Blackjack.Materia.Card,
    module Pitboss.Blackjack.Materia.Hand,
    module Pitboss.Blackjack.Materia.Chips,

    -- * Deck Operations

    -- | Only the most common deck operations are re-exported.
    -- Import "Pitboss.Blackjack.Materia.Deck" for full API.
    Deck,
    mkDeck,
    drawCard,
    shuffle,
    fullDeck,

    -- * Game Rules and Offerings
    module Pitboss.Blackjack.Offering,
    module Pitboss.Blackjack.Offering.Materia,
    module Pitboss.Blackjack.Offering.RuleSet,

    -- * Well-known Configurations
    vegas6,
    downtownSingleDeck,

    -- * Actions and Events
    module Pitboss.Blackjack.Action,

    -- * Game Outcomes
    module Pitboss.Blackjack.Outcome,

    -- * Game Logic
    module Pitboss.Blackjack.Play,
) where

import Pitboss.Blackjack.Materia.Card
import Pitboss.Blackjack.Materia.Chips
import Pitboss.Blackjack.Materia.Hand

import Pitboss.Blackjack.Materia.Deck (Deck, drawCard, fullDeck, mkDeck, shuffle)

import Pitboss.Blackjack.Offering
import Pitboss.Blackjack.Offering.Materia
import Pitboss.Blackjack.Offering.RuleSet

import Pitboss.Blackjack.Offering.WellKnown (downtownSingleDeck, vegas6)

import Pitboss.Blackjack.Action
import Pitboss.Blackjack.Outcome
import Pitboss.Blackjack.Play
