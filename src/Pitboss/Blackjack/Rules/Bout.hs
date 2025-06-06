module Pitboss.Blackjack.Rules.Bout where

import Pitboss.Blackjack.Outcomes
import Pitboss.Blackjack.Types

boutResolution :: SomeHand -> SomeHand -> DetailedOutcome
boutResolution contestantHand dealerHand
    | isBust (handWitness contestantHand) = dealerWinsContestantBust
    | isBust (handWitness dealerHand) = contestantWinsDealerBust
    | isBlackjack (handWitness contestantHand) && not (isBlackjack (handWitness dealerHand)) = contestantWinsBlackjack
    | isBlackjack (handWitness dealerHand) && not (isBlackjack (handWitness contestantHand)) = dealerWinsBlackjack
    | contestantScore > dealerScore = contestantWinsHigher
    | dealerScore > contestantScore = dealerWinsHigher
    | otherwise = pushOutcome
  where
    contestantScore = handScore contestantHand
    dealerScore = handScore dealerHand

simpleBoutResolution :: SomeHand -> SomeHand -> BoutOutcome
simpleBoutResolution contestantHand dealerHand =
    outcome (boutResolution contestantHand dealerHand)
