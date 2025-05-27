{-# LANGUAGE GADTs #-}

module Pitboss.Blackjack.Hand.Operations where

import Pitboss.Blackjack.Card (Card (..), Rank)
import Pitboss.Blackjack.Hand (Hand (..), HandKindWitness (..), HasWitness (witness), SomeHand (..))

extractPairRank :: SomeHand -> Maybe Rank
extractPairRank (SomeHand (Hand cards)) = case cards of
    [Card r1 _, Card r2 _] | r1 == r2 -> Just r1
    _ -> Nothing

cannotSplit :: SomeHand -> Bool
cannotSplit (SomeHand hand) = case witness hand of
    PairWitness -> False
    _ -> True

cannotDouble :: SomeHand -> Bool
cannotDouble (SomeHand hand) = case witness hand of
    BustWitness -> True
    BlackjackWitness -> True
    _ -> False
