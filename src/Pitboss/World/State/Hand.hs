{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.World.State.Hand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Blackjack.Hand.Category (categorize)
import Pitboss.Blackjack.Hand.Category qualified as HC
import Pitboss.Sim.FSM.Hand (HandFSM (..), HandPhase (..), SomeHandFSM (..))
import Pitboss.World.State.Types.Clocked (Clocked (..), Tick)
import Pitboss.World.State.Types.DeltaDriven qualified as DD
import Pitboss.World.State.Types.Reversible (Reversible (..))
import Pitboss.World.State.Types.Snapshot (StateSnapshot (..), applySnapshotDelta)

mkHandState :: Tick -> [Card] -> Chips -> Int -> Int -> HandState
mkHandState t cards bet depth ix =
  HandState
    { _tick = t,
      _handCards = cards,
      _originalBet = bet,
      _splitDepth = depth,
      _handIx = ix
    }

data HandState = HandState
  { _tick :: Tick,
    _handCards :: [Card],
    _originalBet :: Chips,
    _splitDepth :: Int,
    _handIx :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON HandState

instance FromJSON HandState

instance Clocked HandState where
  tick = _tick
  setTick t hs = hs {_tick = t}

data HandDelta
  = AddCard Card
  | RemoveCard Card
  | ReplaceCards [Card] [Card]
  | ReplaceHandIndex Int Int
  | ReplaceSplitDepth Int Int
  deriving (Eq, Show, Generic)

instance ToJSON HandDelta

instance FromJSON HandDelta

instance DD.DeltaDriven HandState HandDelta where
  applyDelta delta hs = case delta of
    AddCard c ->
      hs {_handCards = _handCards hs ++ [c]}
    RemoveCard c ->
      hs {_handCards = filter (/= c) (_handCards hs)}
    ReplaceCards _ new ->
      hs {_handCards = new}
    ReplaceHandIndex _ new ->
      hs {_handIx = new}
    ReplaceSplitDepth _ new ->
      hs {_splitDepth = new}

  previewDelta d hs = Just (DD.applyDelta d hs)

  describeDelta d _ = case d of
    AddCard c -> "Added card: " ++ show c
    RemoveCard c -> "Removed card: " ++ show c
    ReplaceCards old new -> "Replaced cards: " ++ show old ++ " → " ++ show new
    ReplaceHandIndex from to -> "Changed hand index: " ++ show from ++ " → " ++ show to
    ReplaceSplitDepth from to -> "Changed split depth: " ++ show from ++ " → " ++ show to

-- validateDelta d _ =
--   case d of
--     AddCard _ -> Left "AddCard not allowed directly"
--     RemoveCard _ -> Left "RemoveCard not allowed directly"
--     ReplaceCards _ _ -> Left "ReplaceCards not allowed directly"
--     ReplaceHandIndex _ _ -> Right d
--     ReplaceSplitDepth _ _ -> Right d

instance Reversible HandDelta where
  invert = \case
    AddCard c -> Just (RemoveCard c)
    RemoveCard c -> Just (AddCard c)
    ReplaceCards old new -> Just (ReplaceCards new old)
    ReplaceHandIndex from to -> Just (ReplaceHandIndex to from)
    ReplaceSplitDepth from to -> Just (ReplaceSplitDepth to from)

class Validates entity delta err | entity -> delta, entity -> err where
  validateDelta :: delta -> entity -> Either err delta

applyIfValid ::
  (Clocked entity, Validates entity delta err, DD.DeltaDriven entity delta) =>
  (Tick -> Tick) ->
  delta ->
  StateSnapshot entity delta ->
  Either err (StateSnapshot entity delta)
applyIfValid bumpTick delta snapshot@(StateSnapshot entity _) =
  case validateDelta delta entity of
    Left err -> Left err
    Right _ -> Right (applySnapshotDelta bumpTick delta snapshot)

-- instance Validates HandState HandDelta HandDeltaViolation where
--   validateDelta delta _handState =
--     let phase = handPhase delta
--     in validateInPhase phase delta

instance Validates HandState HandDelta HandDeltaViolation where
  validateDelta d hs = case handPhaseFromState hs of
    phase -> case d of
      ReplaceHandIndex _ _ -> Right d
      ReplaceSplitDepth _ _ -> Right d
      AddCard _ -> Left $ IllegalMutation d "cards are already dealt; cannot add more manually" phase
      RemoveCard _ -> Left $ IllegalMutation d "card removal is not allowed during play" phase
      ReplaceCards _ _ -> Left $ IllegalMutation d "card replacement not allowed" phase

handPhaseFromState :: HandState -> HandPhase
handPhaseFromState hs =
  let cards = _handCards hs
      bet = _originalBet hs
   in case categorize cards of
        HC.Blackjack ->
          NaturalBlackjack
        _
          | null cards && bet == 0 ->
              -- No cards and no bet? Possibly uninitialized or awaiting bet.
              Decision
          | length cards == 2 ->
              -- Two cards and bet present? Player decision time.
              Decision
          | length cards > 2 ->
              -- Actively hitting.
              Hitting
          | otherwise ->
              -- Fallback to decision.
              Decision

-- instance Validates HandState HandDelta HandDeltaViolation where
--   validateDelta delta handState =
--     case handFSMFromState handState of
--       Just (SomeHandFSM fsm) -> validateInFSM fsm delta
--       Nothing ->
--         Left $ IllegalMutation
--           { attempted = delta,
--             reason = "could not infer FSM from state",
--             phase = Decision  -- fallback phase
--           }

data HandDeltaViolation
  = IllegalBetChange
      { attempted :: HandDelta,
        reason :: String,
        phase :: HandPhase
      }
  | IllegalMutation
      { attempted :: HandDelta,
        reason :: String,
        phase :: HandPhase
      }
  | IllegalPhase
      { attemptedGroup :: [HandDelta],
        reason :: String,
        phase :: HandPhase
      }
  deriving (Eq, Show)

legalHandDeltas :: SomeHandFSM -> [HandDelta] -> Either HandDeltaViolation [HandDelta]
legalHandDeltas (SomeHandFSM fsm) deltas =
  case fsm of
    AbandonedFSM _ ->
      Left $ IllegalPhase deltas "hand is abandoned" (Abandoned undefined)
    ResolvedFSM _ ->
      Left $ IllegalPhase deltas "hand is resolved" (Resolved undefined)
    BlackjackFSM ->
      Left $ IllegalPhase deltas "hand is a natural blackjack" NaturalBlackjack
    DecisionFSM ->
      mapM (validateInPhase Decision) deltas
    HittingFSM ->
      mapM (validateInPhase Hitting) deltas
    OneCardDrawFSM reason ->
      mapM (validateInPhase (OneCardDraw reason)) deltas

validateInPhase :: HandPhase -> HandDelta -> Either HandDeltaViolation HandDelta
validateInPhase phase d = case d of
  AddCard _ ->
    Left $ IllegalMutation d "cards are already dealt; cannot add more manually" phase
  RemoveCard _ ->
    Left $ IllegalMutation d "card removal is not allowed during play" phase
  ReplaceCards _ _ ->
    Left $ IllegalMutation d "card replacement is disallowed after initial deal" phase
  ReplaceHandIndex _ _ ->
    Right d
  ReplaceSplitDepth _ _ ->
    Right d

-- legalHandDeltas :: SomeHandFSM -> [HandDelta] -> Either HandDeltaViolation [HandDelta]
-- legalHandDeltas (SomeHandFSM fsm) deltas =
--   case fsm of
--     AbandonedFSM _ ->
--       Left $ IllegalPhase deltas "hand is abandoned" (Abandoned undefined)
--     ResolvedFSM _ ->
--       Left $ IllegalPhase deltas "hand is resolved" (Resolved undefined)
--     BlackjackFSM ->
--       Left $ IllegalPhase deltas "hand is a natural blackjack" NaturalBlackjack
--     DecisionFSM ->
--       mapM (validateInPhase Decision) deltas
--     HittingFSM ->
--       mapM (validateInPhase Hitting) deltas
--     OneCardDrawFSM reason ->
--       mapM (validateInPhase (OneCardDraw reason)) deltas
-- legalHandDeltas ::
--   SomeHandFSM ->
--   StateSnapshot HandState HandDelta ->
--   (Tick -> Tick) ->
--   [HandDelta] ->
--   Either HandDeltaViolation (StateSnapshot HandState HandDelta)
-- legalHandDeltas (SomeHandFSM _fsm) snapshot bump =
--   foldl step (Right snapshot)
--   where
--     step acc delta = acc >>= applyIfValid bump delta
-- validateInFSM :: HandFSM p h d s -> HandDelta -> Either HandDeltaViolation HandDelta
-- validateInFSM fsm delta = case fsm of
--   DecisionFSM -> validateDuringDecision
--   HittingFSM -> validateDuringPlay
--   OneCardDrawFSM _ -> validateDuringPlay
--   ResolvedFSM _ -> denyAll "hand is resolved"
--   BlackjackFSM -> denyAll "natural blackjack is terminal"
--   AbandonedFSM _ -> denyAll "hand is abandoned"
--   where
--     deny msg = Left $ IllegalMutation delta msg (handPhase fsm)
--
--     validateDuringDecision = case delta of
--       ReplaceHandIndex _ _ -> Right delta
--       ReplaceSplitDepth _ _ -> Right delta
--       _ -> deny "only index/split metadata allowed in decision phase"
--
--     validateDuringPlay = case delta of
--       ReplaceHandIndex _ _ -> Right delta
--       ReplaceSplitDepth _ _ -> Right delta
--       _ -> deny "cards cannot be modified during live play"
--     denyAll = deny
--
-- handPhase :: HandFSM p h d s -> HandPhase
-- handPhase = \case
--   AbandonedFSM r         -> Abandoned r
--   BlackjackFSM           -> NaturalBlackjack
--   DecisionFSM            -> Decision
--   HittingFSM             -> Hitting
--   OneCardDrawFSM reason  -> OneCardDraw reason
--   ResolvedFSM res        -> Resolved res
