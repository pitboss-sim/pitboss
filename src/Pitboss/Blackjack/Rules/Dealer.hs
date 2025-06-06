{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Blackjack.Rules.Dealer where

import Pitboss.Blackjack.Rules.Game
import Pitboss.Blackjack.Types
import Pitboss.Blackjack.Types.Core

dealerShouldPeek :: GameRuleSet -> Card -> Bool
dealerShouldPeek rules upcard = case holeCardRule rules of
    Peek -> dealerShowsAce upcard || dealerShowsTen upcard
    ENHC -> False

dealerShouldHit :: GameRuleSet -> SomeHand -> Bool
dealerShouldHit ruleset (SomeHand hand) = case witness hand of
    BlackjackWitness -> False
    TwentyOneWitness -> False
    BustWitness -> False
    HardWitness -> handScore (SomeHand hand) < 17
    SoftWitness ->
        let score = handScore (SomeHand hand)
         in score < 17 || (score == 17 && isH17 ruleset)
    PairWitness -> handScore (SomeHand hand) < 17

dealerHasBlackjack :: SomeHand -> Bool
dealerHasBlackjack (SomeHand hand) = case witness hand of
    BlackjackWitness -> True
    _ -> False

dealerShowsAce :: Card -> Bool
dealerShowsAce (Card Ace _) = True
dealerShowsAce _ = False

dealerShowsTen :: Card -> Bool
dealerShowsTen (Card rank _) = rankValue rank == 10

-- incorporate chracterization of hand into this type
dealerUpcard :: SomeHand -> Maybe Card
dealerUpcard (SomeHand hand) = case handCards hand of
    (card : _) -> Just card
    _ -> Nothing
