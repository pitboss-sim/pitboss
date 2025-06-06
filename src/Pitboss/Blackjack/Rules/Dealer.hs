{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Blackjack.Rules.Dealer where

import Pitboss.Blackjack.Rules.Game qualified as Game
import Pitboss.Blackjack.Types.Core
import Pitboss.Blackjack.Types.GameRuleSet
import Pitboss.Blackjack.Types.Hand

dealerShouldPeek :: GameRuleSet -> Card -> Bool
dealerShouldPeek rules upcard = case holeCardRule rules of
    Peek -> dealerShowsAce upcard || dealerShowsTen upcard
    ENHC -> False

dealerShouldHit :: GameRuleSet -> SomeHand -> Bool
dealerShouldHit rules hand = mustHit hand rules

dealerHasBlackjack :: SomeHand -> Bool
dealerHasBlackjack hand = isBlackjack (handWitness hand)

dealerShowsAce :: Card -> Bool
dealerShowsAce (Card Ace _) = True
dealerShowsAce _ = False

dealerShowsTen :: Card -> Bool
dealerShowsTen (Card rank _) = rankValue rank == 10

dealerUpcard :: SomeHand -> Maybe Card
dealerUpcard hand = case handCards hand of
    (card : _) -> Just card
    _ -> Nothing

mustHit :: SomeHand -> GameRuleSet -> Bool
mustHit hand rules =
    let witness = handWitness hand
        score = handScore hand
     in not (isBust witness)
            && not (isBlackjack witness)
            && (score < 17 || (score == 17 && isSoft witness && Game.isH17 rules))
