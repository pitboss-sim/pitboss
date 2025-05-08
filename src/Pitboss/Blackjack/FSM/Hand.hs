{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Blackjack.FSM.Hand where

import Pitboss.Blackjack.Card (Card (..), Rank (..), rankValue)
import Pitboss.Blackjack.FSM.Types.Transitionable
import Pitboss.Blackjack.Hand.Category (HandCategory (..), categorize)
import Pitboss.Blackjack.Hand.Category qualified as HC
import Pitboss.Blackjack.Offering.RuleSet (RuleSet (..), canDouble, canSplitAnotherHand)
import Pitboss.Blackjack.Offering.RuleSet qualified as R

data HandPhase
  = Decision
  | Hitting
  | OneCardDraw OneCardDrawReason
  | Resolved HandResolution
  deriving (Eq, Show)

data HandResolution
  = Surrendered
  | Blackjack
  | Stand
  | Bust
  | SplitNonAces
  | SplitAces
  | Void BankrollImpact -- not surrender; externality
  deriving (Eq, Show)

data OneCardDrawReason = Double | SplitAce
  deriving (Eq, Show)

data BankrollImpact
  = Loss
  | Refund
  deriving (Eq, Show)

data OHit = OKHit | NoHit

data ODbl = OKDbl | NoDbl

data OSpl = OKSpl | NoSpl

data
  HandFSM
    (p :: HandPhase)
    (h :: OHit)
    (d :: ODbl)
    (s :: OSpl)
  where
  DecisionFSM :: HandFSM 'Decision h d s
  HittingFSM :: HandFSM 'Hitting h d s
  ResolvedFSM :: HandResolution -> HandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl

deriving instance Show (HandFSM p h d s)

deriving instance Eq (HandFSM p h d s)

data SomeHandFSM = forall p h d s. SomeHandFSM (HandFSM p h d s)

initialDecision :: HandFSM 'Decision 'OKHit 'OKDbl 'OKSpl
initialDecision = DecisionFSM

resolveSurrender :: HandFSM 'Decision h d s -> HandFSM ('Resolved 'Surrendered) 'NoHit 'NoDbl 'NoSpl
resolveSurrender DecisionFSM = ResolvedFSM Surrendered

toHitting :: HandFSM 'Decision 'OKHit d s -> HandFSM 'Hitting 'OKHit d s
toHitting DecisionFSM = HittingFSM

resolveStand :: HandFSM 'Decision h d s -> HandFSM ('Resolved 'Stand) 'NoHit 'NoDbl 'NoSpl
resolveStand DecisionFSM = ResolvedFSM Stand

resolveBust :: HandFSM 'Hitting h d s -> HandFSM ('Resolved 'Bust) 'NoHit 'NoDbl 'NoSpl
resolveBust HittingFSM = ResolvedFSM Bust

mkInitialHandFSM ::
  RuleSet ->
  -- | first card
  Card ->
  -- | second card
  Card ->
  -- | number of splits so far (0 = initial hand)
  Int ->
  -- | is it a split hand? (affects DAS)
  Bool ->
  SomeHandFSM
mkInitialHandFSM rules c1 c2 splitCount isSplit =
  case categorize [c1, c2] of
    Pair r ->
      let total = rankValue r * 2
          dblAllowed = isDblAllowed total
          isAcePair = r == Ace
          aceOK = not isAcePair || splitAcesAllowed rules == R.SplitAces
          splAllowed = aceOK && canSplitAnotherHand (splitHands rules) splitCount
       in case (dblAllowed, splAllowed) of
            (True, True) -> SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit OKDbl OKSpl)
            (True, False) -> SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit OKDbl NoSpl)
            (False, True) -> SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit NoDbl OKSpl)
            (False, False) -> SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit NoDbl NoSpl)
    Soft2 r ->
      let total = 11 + rankValue r
       in if isDblAllowed total
            then SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit OKDbl NoSpl)
            else SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit NoDbl NoSpl)
    SoftN _ ->
      let ranks = map rank [c1, c2]
          nonAceTotal = sum (map rankValue (filter (/= Ace) ranks))
          total = 11 + nonAceTotal
       in if isDblAllowed total
            then SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit OKDbl NoSpl)
            else SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit NoDbl NoSpl)
    Hard2 r1 r2 ->
      let total = rankValue r1 + rankValue r2
       in if isDblAllowed total
            then SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit OKDbl NoSpl)
            else SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit NoDbl NoSpl)
    HardN _ ->
      let total = handTotalFromCards [c1, c2]
       in if isDblAllowed total
            then SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit OKDbl NoSpl)
            else SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit NoDbl NoSpl)
    HC.TwentyOne ->
      SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit NoDbl NoSpl)
    HC.Blackjack ->
      SomeHandFSM (DecisionFSM :: HandFSM 'Decision NoHit NoDbl NoSpl)
    Partial _ ->
      SomeHandFSM (DecisionFSM :: HandFSM 'Decision OKHit NoDbl NoSpl)
    Empty ->
      error "mkInitialHandFSM: cannot initialize with an empty hand"
  where
    handTotalFromCards :: [Card] -> Int
    handTotalFromCards = sum . map (rankValue . rank)

    isDblAllowed total =
      canDouble (doubling rules) total
        && (not isSplit || das rules == R.DAS)

instance Transitionable (HandFSM p h d s) where
  transitionType = \case
    DecisionFSM -> AwaitInput
    HittingFSM -> AwaitInput
    ResolvedFSM _ -> TerminalPhase
