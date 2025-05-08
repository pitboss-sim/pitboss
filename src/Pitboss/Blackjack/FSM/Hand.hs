{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Blackjack.FSM.Hand where

import Pitboss.Blackjack.Card (Card (..), Rank (..), rankValue)
import Pitboss.Blackjack.FSM.Types.Transitionable
import Pitboss.Blackjack.Hand (Hand, unHand)
import Pitboss.Blackjack.Hand.Category (HandCategory (..), categorize)
import Pitboss.Blackjack.Hand.Category qualified as HC
import Pitboss.Blackjack.Offering.RuleSet (InsuranceOutcome (..), RuleSet (..), Surrender (..), canDouble, canSplitAnotherHand)
import Pitboss.Blackjack.Offering.RuleSet qualified as R

data AbandonedReason
  = Surrender Surrender
  | Insurance InsuranceOutcome
  deriving (Eq, Show)

data HandPhase
  = Abandoned AbandonedReason
  | NaturalBlackjack
  | Decision
  | Hitting
  | OneCardDraw OneCardDrawReason
  | Resolved HandResolution
  deriving (Eq, Show)

data HandResolution
  = Surrendered
  | Blackjack
  | Stand
  | Bust
  | Push
  | SplitNonAces
  | SplitAces
  | DealerBlackjack -- no surrender or ins, dealer autowin
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
  AbandonedFSM :: AbandonedReason -> HandFSM ('Abandoned reason) 'NoHit 'NoDbl 'NoSpl
  BlackjackFSM :: HandFSM 'NaturalBlackjack 'NoHit 'NoDbl 'NoSpl
  DecisionFSM :: HandFSM 'Decision h d s
  HittingFSM :: HandFSM 'Hitting h d s
  OneCardDrawFSM :: OneCardDrawReason -> HandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
  ResolvedFSM :: HandResolution -> HandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl

deriving instance Show (HandFSM p h d s)

deriving instance Eq (HandFSM p h d s)

data SomeHandFSM = forall p h d s. SomeHandFSM (HandFSM p h d s)

initialDecision :: HandFSM 'Decision 'OKHit 'OKDbl 'OKSpl
initialDecision = DecisionFSM

resolveSurrender :: HandFSM 'Decision h d s -> HandFSM ('Resolved 'Surrendered) 'NoHit 'NoDbl 'NoSpl
resolveSurrender DecisionFSM = ResolvedFSM Surrendered

toOneCardDrawFromDecision ::
  OneCardDrawReason ->
  HandFSM 'Decision 'OKHit d s ->
  HandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
toOneCardDrawFromDecision reason DecisionFSM = OneCardDrawFSM reason

toHitting :: HandFSM 'Decision 'OKHit d s -> HandFSM 'Hitting 'OKHit d s
toHitting DecisionFSM = HittingFSM

continueHitting :: HandFSM 'Hitting h d s -> HandFSM 'Hitting h d s
continueHitting HittingFSM = HittingFSM

resolveStand :: HandFSM 'Decision h d s -> HandFSM ('Resolved 'Stand) 'NoHit 'NoDbl 'NoSpl
resolveStand DecisionFSM = ResolvedFSM Stand

resolveBust :: HandFSM 'Hitting h d s -> HandFSM ('Resolved 'Bust) 'NoHit 'NoDbl 'NoSpl
resolveBust HittingFSM = ResolvedFSM Bust

resolveOneCardDraw ::
  HandResolution ->
  HandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl ->
  HandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl
resolveOneCardDraw res (OneCardDrawFSM _) = ResolvedFSM res

resolveSplit :: Hand -> HandFSM 'Decision h d s -> HandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl
resolveSplit hand DecisionFSM =
  case categorize (unHand hand) of
    Pair Ace -> ResolvedFSM SplitAces
    Pair _ -> ResolvedFSM SplitNonAces
    _ -> error "resolveSplit: not a pair"

resolvePush :: -- forced operation
  HandFSM p h d s ->
  HandFSM ('Resolved 'Push) 'NoHit 'NoDbl 'NoSpl
resolvePush _ = ResolvedFSM Push

resolveVoid :: -- forced operation
  BankrollImpact ->
  HandFSM p h d s ->
  HandFSM ('Resolved ('Void impact)) 'NoHit 'NoDbl 'NoSpl
resolveVoid impact _ = ResolvedFSM (Void impact)

resolveDealerBlackjack :: -- forced operation
  HandFSM p h d s ->
  HandFSM ('Resolved 'DealerBlackjack) 'NoHit 'NoDbl 'NoSpl
resolveDealerBlackjack _ = ResolvedFSM DealerBlackjack

mkHandFSMAbandoned :: AbandonedReason -> SomeHandFSM
mkHandFSMAbandoned reason = SomeHandFSM (AbandonedFSM reason)

mkInitialHandFSM ::
  RuleSet ->
  Card ->
  Card ->
  Int ->
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
      SomeHandFSM BlackjackFSM
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
    OneCardDrawFSM _ -> AutoAdvance
    ResolvedFSM _ -> TerminalPhase
    AbandonedFSM _ -> TerminalPhase
    BlackjackFSM -> TerminalPhase -- dealer doesn't have it

isHandTerminal :: HandFSM p h d s -> Bool
isHandTerminal fsm =
  case transitionType fsm of
    TerminalPhase -> True
    _ -> False

resolutionImpact :: HandResolution -> Maybe BankrollImpact
resolutionImpact = \case
  Surrendered -> Just Refund
  Void impact -> Just impact
  Push -> Just Refund
  Bust -> Just Loss
  DealerBlackjack -> Just Loss
  --
  -- Stand, Blackjack, Split*: require comparison, so Maybe
  _ -> Nothing
