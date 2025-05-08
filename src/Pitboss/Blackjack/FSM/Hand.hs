{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Blackjack.FSM.Hand where

import Pitboss.Blackjack.Card (Rank (..))
import Pitboss.Blackjack.FSM.Types.Transitionable
import Pitboss.Blackjack.Hand (Hand (..), unHand)
import Pitboss.Blackjack.Hand.Category qualified as HC
import Pitboss.Blackjack.Hand.Score qualified as HS
import Pitboss.Blackjack.Offering.RuleSet (InsuranceOutcome (..), RuleSet (..), Surrender (..), isH17)

-- shared types

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
  | DealerBlackjack
  | Void BankrollImpact
  deriving (Eq, Show)

data OneCardDrawReason = Double | SplitAce
  deriving (Eq, Show)

data BankrollImpact = Loss | Refund
  deriving (Eq, Show)

data OHit = OKHit | NoHit

data ODbl = OKDbl | NoDbl

data OSpl = OKSpl | NoSpl

-- player hand fsm

data HandFSM (p :: HandPhase) (h :: OHit) (d :: ODbl) (s :: OSpl) where
  AbandonedFSM :: AbandonedReason -> HandFSM ('Abandoned reason) 'NoHit 'NoDbl 'NoSpl
  BlackjackFSM :: HandFSM 'NaturalBlackjack 'NoHit 'NoDbl 'NoSpl
  DecisionFSM :: HandFSM 'Decision h d s
  HittingFSM :: HandFSM 'Hitting h d s
  OneCardDrawFSM :: OneCardDrawReason -> HandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
  ResolvedFSM :: HandResolution -> HandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl

deriving instance Show (HandFSM p h d s)

deriving instance Eq (HandFSM p h d s)

data SomeHandFSM = forall p h d s. SomeHandFSM (HandFSM p h d s)

-- player hand fsm transitions

initialDecision :: HandFSM 'Decision 'OKHit 'OKDbl 'OKSpl
initialDecision = DecisionFSM

resolveSurrender :: HandFSM 'Decision h d s -> HandFSM ('Resolved 'Surrendered) 'NoHit 'NoDbl 'NoSpl
resolveSurrender DecisionFSM = ResolvedFSM Surrendered

toOneCardDrawFromDecision :: OneCardDrawReason -> HandFSM 'Decision 'OKHit d s -> HandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
toOneCardDrawFromDecision reason DecisionFSM = OneCardDrawFSM reason

toHitting :: HandFSM 'Decision 'OKHit d s -> HandFSM 'Hitting 'OKHit d s
toHitting DecisionFSM = HittingFSM

continueHitting :: HandFSM 'Hitting h d s -> HandFSM 'Hitting h d s
continueHitting HittingFSM = HittingFSM

resolveStand :: HandFSM 'Decision h d s -> HandFSM ('Resolved 'Stand) 'NoHit 'NoDbl 'NoSpl
resolveStand DecisionFSM = ResolvedFSM Stand

resolveBust :: HandFSM 'Hitting h d s -> HandFSM ('Resolved 'Bust) 'NoHit 'NoDbl 'NoSpl
resolveBust HittingFSM = ResolvedFSM Bust

resolveOneCardDraw :: HandResolution -> HandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl -> HandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl
resolveOneCardDraw res (OneCardDrawFSM _) = ResolvedFSM res

resolveSplit :: Hand -> HandFSM 'Decision h d s -> HandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl
resolveSplit hand DecisionFSM =
  case HC.categorize (unHand hand) of
    HC.Pair Ace -> ResolvedFSM SplitAces
    HC.Pair _ -> ResolvedFSM SplitNonAces
    _ -> error "resolveSplit: not a pair"

resolvePush, resolveDealerBlackjack :: HandFSM p h d s -> HandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl
resolvePush _ = ResolvedFSM Push
resolveDealerBlackjack _ = ResolvedFSM DealerBlackjack

resolveVoid :: BankrollImpact -> HandFSM p h d s -> HandFSM ('Resolved ('Void impact)) 'NoHit 'NoDbl 'NoSpl
resolveVoid impact _ = ResolvedFSM (Void impact)

mkHandFSMAbandoned :: AbandonedReason -> SomeHandFSM
mkHandFSMAbandoned reason = SomeHandFSM (AbandonedFSM reason)

-- dealer hand fsm

data DealerFSM
  = DealerActing Hand
  | DealerDone HandResolution
  deriving (Eq, Show)

dealerShouldHit :: RuleSet -> Hand -> Bool
dealerShouldHit ruleset hand = case HS.dealerHandTotal hand of
  Nothing -> False -- busted
  Just HS.DealerBlackjack -> False
  Just (HS.Total n soft) -> n < 17 || (n == 17 && soft && isH17 ruleset)

resolveDealerHand :: Hand -> HandResolution
resolveDealerHand hand = case HS.dealerHandTotal hand of
  Nothing -> Bust
  Just HS.DealerBlackjack -> Blackjack
  Just _ -> Stand

-- transitionable instances

instance Transitionable (HandFSM p h d s) where
  transitionType = \case
    DecisionFSM -> AwaitInput
    HittingFSM -> AwaitInput
    OneCardDrawFSM _ -> AutoAdvance
    ResolvedFSM _ -> TerminalPhase
    AbandonedFSM _ -> TerminalPhase
    BlackjackFSM -> TerminalPhase

instance Transitionable DealerFSM where
  transitionType = \case
    DealerActing _ -> AwaitInput
    DealerDone _ -> TerminalPhase

-- helpers

isHandTerminal :: HandFSM p h d s -> Bool
isHandTerminal fsm =
  case transitionType fsm of
    TerminalPhase -> True
    _ -> False

resolutionImpact :: HandResolution -> Maybe BankrollImpact
resolutionImpact = \case
  Surrendered -> Just Refund
  Push -> Just Refund
  Bust -> Just Loss
  DealerBlackjack -> Just Loss
  Void i -> Just i
  _ -> Nothing -- needs dealer comparison
