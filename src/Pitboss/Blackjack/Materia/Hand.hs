{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.Blackjack.Materia.Hand where

import Control.Monad (guard)
import Pitboss.Blackjack.Materia.Instances.Witnessable
import Pitboss.Blackjack.Materia.Types
import Pitboss.Blackjack.Offering.Materia
import Pitboss.Blackjack.Types.Core

mkValidatedHand :: Materia -> [Card] -> Maybe SomeHand
mkValidatedHand matter cards = do
    guard (length cards <= maxCardsFor (matterDecks matter))
    pure (characterize cards)
  where
    maxCardsFor :: DeckCount -> Int
    maxCardsFor d = case d of
        D1 -> 11
        D2 -> 14
        D6 -> 21
        D8 -> 21

handScore :: SomeHand -> Int
handScore (SomeHand hand) = case witness hand of
    BlackjackWitness -> 21
    TwentyOneWitness -> 21
    SoftWitness -> extractScore hand
    HardWitness -> extractScore hand
    PairWitness -> extractScore hand
    BustWitness -> 0

extractScore :: Hand k -> Int
extractScore (Hand cards) = _value (analyzeHand cards)

extractPairRank :: SomeHand -> Maybe Rank
extractPairRank (SomeHand (Hand cards)) = case cards of
    [Card r1 _, Card r2 _] | r1 == r2 -> Just r1
    _ -> Nothing
