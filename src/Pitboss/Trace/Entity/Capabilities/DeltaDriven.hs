{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Trace.Entity.Capabilities.DeltaDriven where

import Pitboss.Blackjack.Hand.Category
import Pitboss.Blackjack.Offering
import Pitboss.Sim.FSM.Hand hiding (Blackjack)
import Pitboss.Trace.Entity.Actor
import Pitboss.Trace.Entity.Hand
import Pitboss.Trace.Entity.Offering
import Pitboss.Trace.Entity.Spot
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.Entity.Types.FiniteMap
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy

class DeltaDriven entity delta | entity -> delta where
  applyDelta :: delta -> entity -> entity
  previewDelta :: delta -> entity -> Maybe entity
  describeDelta :: delta -> entity -> String

-- validateDelta :: delta -> entity -> Either String delta
-- validateDelta d _ = Right d

data TimedDelta d t = TimedDelta {timestamp :: t, delta :: d}

instance DeltaDriven ActorState ActorDelta where
  applyDelta delta = \case
    PlayerActorState t p -> case delta of
      RenameActor name -> PlayerActorState t (p {_playerName = name})
      _ -> PlayerActorState t p
    DealerActorState t d -> case delta of
      RenameActor name -> DealerActorState t (d {_dealerName = name})
      AssignTable tid -> DealerActorState t (d {_assignedTable = Just tid})
      UnassignTable -> DealerActorState t (d {_assignedTable = Nothing})

  describeDelta :: ActorDelta -> entity -> String
  describeDelta d _ = case d of
    RenameActor name -> "Renamed actor to " ++ name
    AssignTable tid -> "Assigned to table " ++ show tid
    UnassignTable -> "Unassigned from table"

  previewDelta d s = Just (applyDelta d s)

instance DeltaDriven OfferingState OfferingDelta where
  applyDelta d os = case d of
    SetMatter m -> os {_offeringMatter = m}
    SetRules r -> os {_offeringRules = r}
    ReplaceOffering (Offering m r) -> os {_offeringMatter = m, _offeringRules = r}

  describeDelta :: OfferingDelta -> entity -> String
  describeDelta d _ = case d of
    SetMatter _ -> "Updated matter config"
    SetRules _ -> "Updated rule set"
    ReplaceOffering _ -> "Replaced full offering"

  previewDelta d os = Just (applyDelta d os)

instance DeltaDriven SpotState SpotDelta where
  applyDelta d ss = case d of
    SetOccupied b -> ss {_spotOccupied = b}
    SetSpotLabel lbl -> ss {_spotLabel = lbl}
    SetHand ix hs ->
      ss {_spotHands = insertFiniteMap ix (Present hs) (_spotHands ss)}
    ClearHand ix ->
      ss {_spotHands = insertFiniteMap ix Absent (_spotHands ss)}

  previewDelta d ss = Just (applyDelta d ss)

  describeDelta d _ = case d of
    SetOccupied b -> "Set occupied to " ++ show b
    SetSpotLabel lbl -> "Set spot label to " ++ lbl
    SetHand ix _ -> "Set hand " ++ show ix
    ClearHand ix -> "Cleared hand " ++ show ix

instance DeltaDriven HandState HandDelta where
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

  previewDelta d hs = Just (applyDelta d hs)

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

class Validates entity delta err | entity -> delta, entity -> err where
  validateDelta :: delta -> entity -> Either err delta

-- applyIfValid ::
--   (Clocked entity, Validates entity delta err, DeltaDriven entity delta) =>
--   (Tick -> Tick) ->
--   delta ->
--   StateSnapshot entity delta ->
--   Either err (StateSnapshot entity delta)
-- applyIfValid bumpTick delta snapshot@(StateSnapshot entity _) =
--   case validateDelta delta entity of
--     Left err -> Left err
--     Right _ -> Right (applySnapshotDelta bumpTick delta snapshot)
--
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
        Blackjack ->
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

instance DeltaDriven TableState TableDelta where
  applyDelta delta ts = case delta of
    SetOffering ref -> ts {_offeringUsed = ref}
    SetMinBet amt -> ts {_minBet = amt}
    SetTableName name -> ts {_tableName = name}
    StartRound ref -> ts {_currentRound = Just ref}
    EndRound -> ts {_currentRound = Nothing}

  describeDelta delta _ = case delta of
    SetOffering _ -> "Set offering (frozen ref)"
    SetMinBet amt -> "Set minimum bet to " ++ show amt
    SetTableName name -> "Set table name to " ++ name
    StartRound rid -> "Started round " ++ show rid
    EndRound -> "Ended current round"

  previewDelta delta ts = Just (applyDelta delta ts)
