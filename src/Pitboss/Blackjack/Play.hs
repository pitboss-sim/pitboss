{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Play where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack.Rules.Hand
import Pitboss.Blackjack.Types.Core
import Pitboss.Blackjack.Types.Hand
import Pitboss.Blackjack.Types.Hand.Witness

data HandProgress = Empty | Partial | Full | ActedUpon
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

getProgress :: SomeHand -> HandProgress
getProgress hand =
    case completeness (handWitness hand) of
        NoneWitness -> Empty
        PartialWitness -> Partial
        FullWitness -> Full
        ExtendedWitness -> ActedUpon

createEmptyHand :: SomeHand
createEmptyHand = characterize []

dealFirstCard :: SomeHand -> Card -> SomeHand
dealFirstCard hand card =
    case handCards hand of
        [] -> characterize [card]
        _ -> error "dealFirstCard: hand is not empty"

dealSecondCard :: SomeHand -> Card -> SomeHand
dealSecondCard hand card =
    case handCards hand of
        [c1] -> characterize [c1, card]
        _ -> error "dealSecondCard: hand does not have exactly one card"

hit :: SomeHand -> Card -> SomeHand
hit hand card = characterize $ handCards hand ++ [card]

stand :: SomeHand -> SomeHand
stand hand = hand

double :: SomeHand -> Card -> SomeHand
double hand card = characterize $ handCards hand ++ [card]

split :: SomeHand -> (SomeHand, SomeHand)
split hand = case handCards hand of
    [card1, card2] -> (characterize [card1], characterize [card2])
    _ -> error "split: not a two-card hand"
