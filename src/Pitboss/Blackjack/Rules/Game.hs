module Pitboss.Blackjack.Rules.Game where

import Pitboss.Blackjack.Types.GameRuleSet

isH17 :: GameRuleSet -> Bool
isH17 rs = case soft17 rs of
    HitSoft17 -> True
    StandSoft17 -> False

canSplitAnotherHand :: SplitHands -> Int -> Bool
canSplitAnotherHand limit current = current < maxSplits limit
  where
    maxSplits :: SplitHands -> Int
    maxSplits SP2 = 2
    maxSplits SP3 = 3
    maxSplits SP4 = 4
