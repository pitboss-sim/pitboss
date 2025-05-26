{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.Blackjack.Play where

import Pitboss.Blackjack.Materia.Card (Card, Rank (..))
import Pitboss.Blackjack.Materia.Hand (HandKindWitness (..), SomeHand (..), characterize, extractPairRank, handScore, unHand, witness)
import Pitboss.Blackjack.Offering (Offering, ruleSet)
import Pitboss.Blackjack.Offering.RuleSet (DoubleRule (..), ResplitAcesAllowed (..), RuleSet, SplitAcesAllowed (..), canSplitAnotherHand, doubling, resplitAcesAllowed, splitAcesAllowed, splitHands)

data HandPhase = Empty | Partial | Full | ActedUpon
    deriving (Eq, Show)

data LifecycleHand (phase :: HandPhase) where
    EmptyLifecycleHand :: LifecycleHand 'Empty
    PartialLifecycleHand :: [Card] -> LifecycleHand 'Partial
    FullLifecycleHand :: [Card] -> LifecycleHand 'Full
    ActedUponLifecycleHand :: [Card] -> LifecycleHand 'ActedUpon

data SomeLifecycleHand = forall phase. SomeLifecycleHand (LifecycleHand phase)

fromSomeHand :: SomeHand -> SomeLifecycleHand
fromSomeHand (SomeHand hand) =
    let cards = unHand hand
     in case length cards of
            0 -> SomeLifecycleHand EmptyLifecycleHand
            1 -> SomeLifecycleHand (PartialLifecycleHand cards)
            2 -> SomeLifecycleHand (FullLifecycleHand cards)
            _ -> SomeLifecycleHand (ActedUponLifecycleHand cards)

deriving instance Show (LifecycleHand phase)
deriving instance Eq (LifecycleHand phase)

type family CanCharacterize (phase :: HandPhase) :: Bool where
    CanCharacterize 'Full = 'True
    CanCharacterize 'ActedUpon = 'True
    CanCharacterize _ = 'False

type family CanDouble (phase :: HandPhase) :: Bool where
    CanDouble 'Full = 'True
    CanDouble _ = 'False

type family CanSplit (phase :: HandPhase) :: Bool where
    CanSplit 'Full = 'True
    CanSplit _ = 'False

data Outcome
    = PlayerWins
    | DealerWins
    | Push
    | PlayerBusts
    | DealerBusts
    deriving (Eq, Show)

extractCards :: LifecycleHand phase -> [Card]
extractCards EmptyLifecycleHand = []
extractCards (PartialLifecycleHand cards) = cards
extractCards (FullLifecycleHand cards) = cards
extractCards (ActedUponLifecycleHand cards) = cards

isBusted :: SomeHand -> Bool
isBusted hand = handScore hand > 21

characterizeHand :: (CanCharacterize phase ~ 'True) => LifecycleHand phase -> SomeHand
characterizeHand hand = characterize (extractCards hand)

boutResolution :: SomeHand -> SomeHand -> Outcome
boutResolution playerHand dealerHand
    | isBusted playerHand = PlayerBusts
    | isBusted dealerHand = DealerBusts
    | playerScore > dealerScore = PlayerWins
    | dealerScore > playerScore = DealerWins
    | otherwise = Push
  where
    playerScore = handScore playerHand
    dealerScore = handScore dealerHand

canDoubleHand :: (CanDouble phase ~ 'True, CanCharacterize phase ~ 'True) => LifecycleHand phase -> Offering -> Bool
canDoubleHand hand offering =
    canDoubleWithRules (ruleSet offering) (handScore (characterizeHand hand))

canDoubleSomeHand :: SomeHand -> Offering -> Bool
canDoubleSomeHand hand offering =
    case fromSomeHand hand of
        SomeLifecycleHand fullHand@(FullLifecycleHand _) -> canDoubleHand fullHand offering
        SomeLifecycleHand (ActedUponLifecycleHand _) -> False
        _ -> False

canSplitHand :: (CanSplit phase ~ 'True, CanCharacterize phase ~ 'True) => LifecycleHand phase -> Int -> Offering -> Bool
canSplitHand hand splitCount offering =
    let someHand = characterizeHand hand
     in case someHand of
            SomeHand h -> case witness h of
                PairWitness -> canSplitWithRules (ruleSet offering) someHand splitCount
                _ -> False

canSplitSomeHand :: SomeHand -> Int -> Offering -> Bool
canSplitSomeHand hand splitCount offering =
    case fromSomeHand hand of
        SomeLifecycleHand fullHand@(FullLifecycleHand _) -> canSplitHand fullHand splitCount offering
        SomeLifecycleHand (ActedUponLifecycleHand _) -> False
        _ -> False

canDoubleWithRules :: RuleSet -> Int -> Bool
canDoubleWithRules rules total = case doubling rules of
    DoubleAny -> True
    Double9_10 -> total == 9 || total == 10
    Double9_11 -> total >= 9 && total <= 11
    Double10_11 -> total == 10 || total == 11

canSplitWithRules :: RuleSet -> SomeHand -> Int -> Bool
canSplitWithRules rules hand splitCount =
    let withinSplitLimit = canSplitAnotherHand (splitHands rules) splitCount
     in if isPairOfAces hand
            then
                withinSplitLimit
                    && (splitAcesAllowed rules == SplitAces)
                    && (splitCount == 0 || resplitAcesAllowed rules == ResplitAces)
            else withinSplitLimit

isPairOfAces :: SomeHand -> Bool
isPairOfAces hand = case extractPairRank hand of
    Just Ace -> True
    _ -> False

createEmptyLifecycleHand :: LifecycleHand 'Empty
createEmptyLifecycleHand = EmptyLifecycleHand

dealFirstCard :: LifecycleHand 'Empty -> Card -> LifecycleHand 'Partial
dealFirstCard EmptyLifecycleHand card = PartialLifecycleHand [card]

dealSecondCard :: LifecycleHand 'Partial -> Card -> LifecycleHand 'Full
dealSecondCard (PartialLifecycleHand cards) card = FullLifecycleHand (cards ++ [card])

hit :: LifecycleHand 'Full -> Card -> LifecycleHand 'ActedUpon
hit (FullLifecycleHand cards) card = ActedUponLifecycleHand (cards ++ [card])

hitAgain :: LifecycleHand 'ActedUpon -> Card -> LifecycleHand 'ActedUpon
hitAgain (ActedUponLifecycleHand cards) card = ActedUponLifecycleHand (cards ++ [card])

stand :: LifecycleHand 'Full -> LifecycleHand 'Full
stand hand = hand

standAfterHit :: LifecycleHand 'ActedUpon -> LifecycleHand 'ActedUpon
standAfterHit hand = hand

double :: LifecycleHand 'Full -> Card -> LifecycleHand 'ActedUpon
double (FullLifecycleHand cards) card = ActedUponLifecycleHand (cards ++ [card])

split :: LifecycleHand 'Full -> (LifecycleHand 'Partial, LifecycleHand 'Partial)
split (FullLifecycleHand [card1, card2]) = (PartialLifecycleHand [card1], PartialLifecycleHand [card2])
split _ = error "split: not a two-card hand"
