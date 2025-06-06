module Pitboss.Blackjack.Rules.Bout where

import Pitboss.Blackjack.Outcomes
import Pitboss.Blackjack.Types

boutResolution :: SomeHand -> SomeHand -> DetailedOutcome
boutResolution boutPlayerHand dealerHand
    | isBust (handWitness boutPlayerHand) = dealerWinsBoutPlayerBust
    | isBust (handWitness dealerHand) = boutPlayerWinsDealerBust
    | isBlackjack (handWitness boutPlayerHand) && not (isBlackjack (handWitness dealerHand)) = boutPlayerWinsBlackjack
    | isBlackjack (handWitness dealerHand) && not (isBlackjack (handWitness boutPlayerHand)) = dealerWinsBlackjack
    | boutPlayerScore > dealerScore = boutPlayerWinsHigher
    | dealerScore > boutPlayerScore = dealerWinsHigher
    | otherwise = pushOutcome
  where
    boutPlayerScore = handScore boutPlayerHand
    dealerScore = handScore dealerHand

simpleBoutResolution :: SomeHand -> SomeHand -> BoutOutcome
simpleBoutResolution boutPlayerHand dealerHand =
    outcome (boutResolution boutPlayerHand dealerHand)
