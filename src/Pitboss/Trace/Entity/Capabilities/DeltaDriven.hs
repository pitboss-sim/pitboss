{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Capabilities.DeltaDriven where

import Pitboss.FSM.PlayerSpotFSM
import Pitboss.Trace.Entity.Dealer
import Pitboss.Trace.Entity.Dealer qualified as D
import Pitboss.Trace.Entity.DealerHand
import Pitboss.Trace.Entity.DealerHand qualified as DH
import Pitboss.Trace.Entity.DealerRound
import Pitboss.Trace.Entity.DealerRound qualified as DR
import Pitboss.Trace.Entity.Offering
import Pitboss.Trace.Entity.Offering qualified as O
import Pitboss.Trace.Entity.Player
import Pitboss.Trace.Entity.Player qualified as P
import Pitboss.Trace.Entity.PlayerHand
import Pitboss.Trace.Entity.PlayerHand qualified as PH
import Pitboss.Trace.Entity.PlayerSpot
import Pitboss.Trace.Entity.PlayerSpot qualified as PS
import Pitboss.Trace.Entity.Shoe
import Pitboss.Trace.Entity.ShoeCursor
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.Entity.Table qualified as T
import Pitboss.Trace.Entity.Types.FiniteMap

type DeltaFor entity = Delta entity

class DeltaDriven entity where
  type Delta entity
  applyDelta :: Delta entity -> entity -> entity
  previewDelta :: Delta entity -> entity -> Maybe entity
  describeDelta :: Delta entity -> entity -> String

data TimedDelta d t = TimedDelta {timestamp :: t, delta :: d}

instance DeltaDriven DealerEntity where
  type Delta DealerEntity = DealerEntityDelta

  applyDelta = applyDealerEntityDelta

  previewDelta :: DealerEntityDelta -> DealerEntity -> Maybe DealerEntity
  previewDelta delta entity = case delta of
    DealerStateDelta sd -> case sd of
      RenameDealer _ _ -> Just $ applyDealerEntityDelta delta entity
      ReplaceAssignedTable old _ ->
        if _assignedTable (D._state entity) == old
          then Just $ applyDealerEntityDelta delta entity
          else Nothing
    DealerRelationsDelta rd -> case rd of
      D.UpdateRound old _ ->
        if D._currentRound (D._rels entity) == old
          then Just $ applyDealerEntityDelta delta entity
          else Nothing
      D.UpdateHand old _ ->
        if _activeHand (D._rels entity) == old
          then Just $ applyDealerEntityDelta delta entity
          else Nothing
    DealerFSMDelta _ -> Just $ applyDealerEntityDelta delta entity

  describeDelta d _ = case d of
    D.DealerStateDelta sd -> case sd of
      D.RenameDealer from to ->
        "Renamed dealer: " ++ from ++ " → " ++ to
      D.ReplaceAssignedTable from to ->
        "Reassigned dealer table: " ++ show from ++ " → " ++ show to
    D.DealerRelationsDelta rd -> case rd of
      D.UpdateRound from to ->
        "Updated dealer round: " ++ show from ++ " → " ++ show to
      D.UpdateHand from to ->
        "Updated dealer hand: " ++ show from ++ " → " ++ show to
    D.DealerFSMDelta fd -> case fd of
      D.ReplaceTableFSM _ _ ->
        "Replaced DealerTableFSM"
      D.ReplaceRoundFSM _ _ ->
        "Replaced DealerRoundFSM"
      D.ReplaceHandFSM _ _ ->
        "Replaced DealerHandFSM"

instance DeltaDriven DealerHandEntity where
  type Delta DealerHandEntity = DealerHandEntityDelta

  applyDelta delta entity = case delta of
    DealerHandStateDelta d ->
      entity {DH._state = applyDealerHandStateDelta d (DH._state entity)}
    DealerHandRelationsDelta d ->
      entity {DH._rels = applyDealerHandRelationsDelta d (DH._rels entity)}
    DealerHandFSMDelta d ->
      entity {DH._fsm = applyDealerHandFSMDelta d (DH._fsm entity)}

  previewDelta delta entity = case delta of
    DealerHandStateDelta d -> case d of
      ReplaceDealerHandIndex old _ ->
        if old == DH._handIx (DH._state entity)
          then Just (applyDealerHandEntityDelta delta entity)
          else Nothing
      DH.ReplaceSplitDepth old _ ->
        if old == DH._splitDepth (DH._state entity)
          then Just (applyDealerHandEntityDelta delta entity)
          else Nothing
      _ -> Just (applyDealerHandEntityDelta delta entity)
    DealerHandRelationsDelta d -> case d of
      DH.UpdatePlayerSpot old _ ->
        if old == DH._belongsToPlayerSpot (DH._rels entity)
          then Just (applyDealerHandEntityDelta delta entity)
          else Nothing
      DH.UpdateRound old _ ->
        if old == DH._belongsToRound (DH._rels entity)
          then Just (applyDealerHandEntityDelta delta entity)
          else Nothing
      UpdateDealer old _ ->
        if old == _ownedByDealer (DH._rels entity)
          then Just (applyDealerHandEntityDelta delta entity)
          else Nothing
    DealerHandFSMDelta _ -> Just (applyDealerHandEntityDelta delta entity)

  describeDelta d _ = case d of
    DealerHandStateDelta sd -> describeDealerHandStateDelta sd
    DealerHandRelationsDelta rd -> describeDealerHandRelationsDelta rd
    DealerHandFSMDelta _ -> "FSM replaced"

describeDealerHandStateDelta :: DealerHandStateDelta -> String
describeDealerHandStateDelta = \case
  DH.AddCard c -> "Added card: " ++ show c
  DH.RemoveCard c -> "Removed card: " ++ show c
  DH.ReplaceCards old new -> "Replaced cards: " ++ show old ++ " → " ++ show new
  DH.ReplaceDealerHandIndex from to -> "Changed hand index: " ++ show from ++ " → " ++ show to
  DH.ReplaceSplitDepth from to -> "Changed split depth: " ++ show from ++ " → " ++ show to

describeDealerHandRelationsDelta :: DealerHandRelationsDelta -> String
describeDealerHandRelationsDelta = \case
  DH.UpdatePlayerSpot from to -> "Changed spot: " ++ show from ++ " → " ++ show to
  DH.UpdateRound from to -> "Changed round: " ++ show from ++ " → " ++ show to
  DH.UpdateDealer from to -> "Changed dealer: " ++ show from ++ " → " ++ show to

instance DeltaDriven DealerRoundEntity where
  type Delta DealerRoundEntity = DealerRoundEntityDelta
  applyDelta = applyDealerRoundEntityDelta
  previewDelta d e = Just (applyDealerRoundEntityDelta d e)
  describeDelta d _ = case d of
    DealerRoundStateDelta (SetDealerRoundNumber n) -> "Set round number to " ++ show n
    DealerRoundStateDelta (SetActive b) -> "Set round active: " ++ show b
    DealerRoundRelationsDelta _ -> "Updated shoe used"

applyDealerRoundStateDelta :: DealerRoundStateDelta -> DealerRoundState -> DealerRoundState
applyDealerRoundStateDelta delta s = case delta of
  SetDealerRoundNumber n -> s {_roundNumber = n}
  SetActive b -> s {_isActive = b}

applyDealerRoundRelationsDelta :: DealerRoundRelationsDelta -> DealerRoundRelations -> DealerRoundRelations
applyDealerRoundRelationsDelta (SetShoeUsed shoe) r = r {_shoeUsed = shoe}

applyDealerRoundEntityDelta :: DealerRoundEntityDelta -> DealerRoundEntity -> DealerRoundEntity
applyDealerRoundEntityDelta delta e = case delta of
  DealerRoundStateDelta d -> e {DR._state = applyDealerRoundStateDelta d (DR._state e)}
  DealerRoundRelationsDelta d -> e {DR._rels = applyDealerRoundRelationsDelta d (DR._rels e)}

instance DeltaDriven OfferingEntity where
  type Delta OfferingEntity = OfferingEntityDelta

  applyDelta = applyOfferingEntityDelta

  previewDelta delta entity = case delta of
    O.OfferingStateDelta d -> case d of
      O.ReplaceOffering old _ ->
        if old == O._offering (O._state entity)
          then Just (applyOfferingEntityDelta delta entity)
          else Nothing
    O.OfferingRelationsDelta d -> case d of
      O.AddTable tid ->
        if tid `notElem` O._associatedTables (O._rels entity)
          then Just (applyOfferingEntityDelta delta entity)
          else Nothing
      O.RemoveTable tid ->
        if tid `elem` O._associatedTables (O._rels entity)
          then Just (applyOfferingEntityDelta delta entity)
          else Nothing

  describeDelta d _ = case d of
    O.OfferingStateDelta sd -> describeOfferingStateDelta sd
    O.OfferingRelationsDelta rd -> describeOfferingRelationsDelta rd

describeOfferingStateDelta :: OfferingStateDelta -> String
describeOfferingStateDelta = \case
  O.ReplaceOffering _ _ ->
    "Replaced offering (details omitted)"

describeOfferingRelationsDelta :: OfferingRelationsDelta -> String
describeOfferingRelationsDelta = \case
  O.AddTable tid ->
    "Added table to offering: " ++ show tid
  O.RemoveTable tid ->
    "Removed table from offering: " ++ show tid

instance DeltaDriven PlayerEntity where
  type Delta PlayerEntity = PlayerDelta
  applyDelta d e = e {P._state = applyPlayerDelta d (P._state e)}
  previewDelta d e = Just (applyDelta d e)
  describeDelta d _ = case d of
    RenamePlayer name -> "Renamed player to " ++ name
    SetBankroll chips -> "Set bankroll to " ++ show chips

applyPlayerDelta :: PlayerDelta -> PlayerState -> PlayerState
applyPlayerDelta d s = case d of
  RenamePlayer name -> s {_playerName = name}
  SetBankroll c -> s {_bankroll = c}

instance DeltaDriven PlayerHandEntity where
  type Delta PlayerHandEntity = PlayerHandEntityDelta

  applyDelta delta entity = case delta of
    PlayerHandStateDelta d ->
      entity {PH._state = applyPlayerHandStateDelta d (PH._state entity)}
    PlayerHandRelationsDelta d ->
      entity {PH._rels = applyPlayerHandRelationsDelta d (PH._rels entity)}
    PlayerHandFSMDelta d ->
      entity {PH._fsm = applyPlayerHandFSMDelta d (PH._fsm entity)}

  previewDelta delta entity = case delta of
    PlayerHandStateDelta d -> case d of
      PH.ReplacePlayerHandIndex old _ ->
        if old == PH._handIx (PH._state entity)
          then Just (applyPlayerHandEntityDelta delta entity)
          else Nothing
      PH.ReplaceSplitDepth old _ ->
        if old == PH._splitDepth (PH._state entity)
          then Just (applyPlayerHandEntityDelta delta entity)
          else Nothing
      _ -> Just (applyPlayerHandEntityDelta delta entity)
    PlayerHandRelationsDelta d -> case d of
      PH.UpdatePlayerSpot old _ ->
        if old == PH._belongsToPlayerSpot (PH._rels entity)
          then Just (applyPlayerHandEntityDelta delta entity)
          else Nothing
      PH.UpdateRound old _ ->
        if old == PH._belongsToRound (PH._rels entity)
          then Just (applyPlayerHandEntityDelta delta entity)
          else Nothing
      PH.UpdatePlayer old _ ->
        if old == _ownedByPlayer (PH._rels entity)
          then Just (applyPlayerHandEntityDelta delta entity)
          else Nothing
    PlayerHandFSMDelta _ -> Just (applyPlayerHandEntityDelta delta entity)

  describeDelta d _ = case d of
    PH.PlayerHandStateDelta sd -> describePlayerHandStateDelta sd
    PH.PlayerHandRelationsDelta rd -> describePlayerHandRelationsDelta rd
    PH.PlayerHandFSMDelta _ -> "FSM replaced"

describePlayerHandStateDelta :: PlayerHandStateDelta -> String
describePlayerHandStateDelta = \case
  PH.AddCard c -> "Added card: " ++ show c
  PH.RemoveCard c -> "Removed card: " ++ show c
  PH.ReplaceCards old new -> "Replaced cards: " ++ show old ++ " → " ++ show new
  PH.ReplacePlayerHandIndex from to -> "Changed hand index: " ++ show from ++ " → " ++ show to
  PH.ReplaceSplitDepth from to -> "Changed split depth: " ++ show from ++ " → " ++ show to

describePlayerHandRelationsDelta :: PlayerHandRelationsDelta -> String
describePlayerHandRelationsDelta = \case
  PH.UpdateRound from to -> "Changed round: " ++ show from ++ " → " ++ show to
  PH.UpdatePlayer from to -> "Changed player: " ++ show from ++ " → " ++ show to
  PH.UpdatePlayerSpot from to -> "Changed spot: " ++ show from ++ " → " ++ show to

instance DeltaDriven PlayerSpotEntity where
  type Delta PlayerSpotEntity = PlayerSpotEntityDelta

  applyDelta = applyPlayerSpotEntityDelta

  previewDelta delta entity = case delta of
    PlayerSpotStateDelta sd -> case sd of
      ReplaceWager old _ ->
        if _wager (PS._state entity) == old
          then Just (applyPlayerSpotEntityDelta delta entity)
          else Nothing
      UpdateHandOccupancy (oldK, oldV) _ ->
        case lookupFiniteMap oldK (_handOccupancy (PS._state entity)) of
          Just actualV
            | actualV == oldV ->
                Just (applyPlayerSpotEntityDelta delta entity)
          _ -> Nothing
    PlayerSpotRelationsDelta rd -> case rd of
      PS.UpdatePlayer old _ ->
        if _playerId (PS._rels entity) == old
          then Just (applyPlayerSpotEntityDelta delta entity)
          else Nothing
      PS.UpdateRound old _ ->
        if _roundId (PS._rels entity) == old
          then Just (applyPlayerSpotEntityDelta delta entity)
          else Nothing
    PlayerSpotFSMDelta _ ->
      Just (applyPlayerSpotEntityDelta delta entity)

  describeDelta d _ = case d of
    PlayerSpotStateDelta sd -> describePlayerSpotStateDelta sd
    PlayerSpotRelationsDelta rd -> describePlayerSpotRelationsDelta rd
    PlayerSpotFSMDelta fd -> describePlayerSpotFSMDelta fd

describePlayerSpotStateDelta :: PlayerSpotStateDelta -> String
describePlayerSpotStateDelta = \case
  ReplaceWager from to ->
    "Replaced wager: " ++ show from ++ " → " ++ show to
  UpdateHandOccupancy (k1, v1) (k2, v2) ->
    "Updated hand occupancy: " ++ show k1 ++ "=" ++ show v1 ++ " → " ++ show k2 ++ "=" ++ show v2

describePlayerSpotRelationsDelta :: PlayerSpotRelationsDelta -> String
describePlayerSpotRelationsDelta = \case
  PS.UpdatePlayer from to -> "Updated player ID: " ++ show from ++ " → " ++ show to
  PS.UpdateRound from to -> "Updated round ID: " ++ show from ++ " → " ++ show to

describePlayerSpotFSMDelta :: PlayerSpotFSMDelta -> String
describePlayerSpotFSMDelta _ = "Replaced PlayerSpot FSM"

applyPlayerSpotStateDelta :: PlayerSpotStateDelta -> PlayerSpotState -> PlayerSpotState
applyPlayerSpotStateDelta delta state = case delta of
  ReplaceWager _ new -> state {_wager = new}
  UpdateHandOccupancy (_, _) (k, v) ->
    state {_handOccupancy = insertFiniteMap k v (_handOccupancy state)}

applyPlayerSpotRelationsDelta :: PlayerSpotRelationsDelta -> PlayerSpotEntityRelations -> PlayerSpotEntityRelations
applyPlayerSpotRelationsDelta delta rels = case delta of
  PS.UpdatePlayer _ new -> rels {_playerId = new}
  PS.UpdateRound _ new -> rels {_roundId = new}

applyPlayerSpotFSMDelta :: PlayerSpotFSMDelta -> SomePlayerSpotFSM -> SomePlayerSpotFSM
applyPlayerSpotFSMDelta (PS.ReplaceFSM _ new) _ = new

applyPlayerSpotEntityDelta :: PlayerSpotEntityDelta -> PlayerSpotEntity -> PlayerSpotEntity
applyPlayerSpotEntityDelta delta entity = case delta of
  PlayerSpotStateDelta sd ->
    entity {PS._state = applyPlayerSpotStateDelta sd (PS._state entity)}
  PlayerSpotRelationsDelta rd ->
    entity {PS._rels = applyPlayerSpotRelationsDelta rd (PS._rels entity)}
  PlayerSpotFSMDelta fd ->
    entity {PS._fsm = applyPlayerSpotFSMDelta fd (PS._fsm entity)}

instance DeltaDriven TableEntity where
  type Delta TableEntity = TableEntityDelta

  applyDelta = applyTableEntityDelta

  previewDelta delta entity = case delta of
    TableStateDelta sd -> case sd of
      SetTableName old _ ->
        if old == _tableName (T._state entity)
          then Just (applyTableEntityDelta delta entity)
          else Nothing
      SetMinBet old _ ->
        if old == _minBet (T._state entity)
          then Just (applyTableEntityDelta delta entity)
          else Nothing
      SetOffering old _ ->
        if old == _offeringUsed (T._state entity)
          then Just (applyTableEntityDelta delta entity)
          else Nothing
      StartRound prev _ ->
        if T._currentRound (T._state entity) == prev
          then Just (applyTableEntityDelta delta entity)
          else Nothing
      EndRound old ->
        if T._currentRound (T._state entity) == Just old
          then Just (applyTableEntityDelta delta entity)
          else Nothing
    TableRelationsDelta rd -> case rd of
      AssignDealer prev _ ->
        if _managedByDealer (T._rels entity) == prev
          then Just (applyTableEntityDelta delta entity)
          else Nothing
      UnassignDealer old ->
        if _managedByDealer (T._rels entity) == Just old
          then Just (applyTableEntityDelta delta entity)
          else Nothing

  describeDelta d _ = case d of
    TableStateDelta sd -> case sd of
      SetTableName from to ->
        "Changed table name: " ++ from ++ " → " ++ to
      SetMinBet from to ->
        "Changed minimum bet: " ++ show from ++ " → " ++ show to
      SetOffering from to ->
        "Changed offering: " ++ show from ++ " → " ++ show to
      StartRound prev new ->
        "Started round: " ++ show new ++ maybe "" (\p -> " (previous: " ++ show p ++ ")") prev
      EndRound old ->
        "Ended round: " ++ show old
    TableRelationsDelta rd -> case rd of
      AssignDealer prev new ->
        "Assigned dealer: " ++ maybe "None" show prev ++ " → " ++ show new
      UnassignDealer old ->
        "Unassigned dealer: " ++ show old

instance DeltaDriven ShoeCursorEntity where
  type Delta ShoeCursorEntity = ShoeCursorEntityDelta

  applyDelta = applyShoeCursorEntityDelta
  previewDelta d e = Just (applyDelta d e)
  describeDelta d _ = case d of
    ShoeCursorStateDelta sd -> case sd of
      Advance n -> "Advanced cursor by " ++ show n
      Rewind n -> "Rewound cursor by " ++ show n
      ReplaceOffset old new -> "Offset: " ++ show old ++ " → " ++ show new
    ShoeCursorRelationsDelta (UpdateShoe old new) ->
      "Updated cursor shoe ref: " ++ show old ++ " → " ++ show new

instance DeltaDriven ShoeEntity where
  type Delta ShoeEntity = ShoeEntityDelta
  applyDelta _ s = s
  describeDelta _ _ = "No-op delta for ShoeEntity"
  previewDelta _ = Just

-- validateDelta d _ =
--   case d of
--     AddCard _ -> Left "AddCard not allowed directly"
--     RemoveCard _ -> Left "RemoveCard not allowed directly"
--     ReplaceCards _ _ -> Left "ReplaceCards not allowed directly"
--     ReplacePlayerHandIndex _ _ -> Right d
--     ReplaceSplitDepth _ _ -> Right d

-- class Validates entity delta err | entity -> delta, entity -> err where
--   validateDelta :: delta -> entity -> Either err delta
--
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
-- instance Validates PlayerHandState PlayerHandStateDelta PlayerHandStateDeltaViolation where
--   validateDelta delta _handState =
--     let phase = handPhase delta
--     in validateInPhase phase delta

-- instance Validates PlayerHandState PlayerHandStateDelta PlayerHandStateDeltaViolation where
--   validateDelta d hs = case handPhaseFromState hs of
--     phase -> case d of
--       PH.ReplacePlayerHandIndex _ _ -> Right d
--       PH.ReplaceSplitDepth _ _ -> Right d
--       PH.AddCard _ -> Left $ IllegalMutation d "cards are already dealt; cannot add more manually" phase
--       PH.RemoveCard _ -> Left $ IllegalMutation d "card removal is not allowed during play" phase
--       PH.ReplaceCards _ _ -> Left $ IllegalMutation d "card replacement not allowed" phase
--
-- handPhaseFromState :: PlayerHandState -> HandPhase
-- handPhaseFromState hs =
--   let cards = PH._handCards hs
--       bet = PH._originalBet hs
--    in case categorize cards of
--         Blackjack ->
--           NaturalBlackjack
--         _
--           | null cards && bet == 0 ->
--               -- No cards and no bet? Possibly uninitialized or awaiting bet.
--               Decision
--           | length cards == 2 ->
--               -- Two cards and bet present? Player decision time.
--               Decision
--           | length cards > 2 ->
--               -- Actively hitting.
--               Hitting
--           | otherwise ->
--               -- Fallback to decision.
--               Decision
--
-- data PlayerHandStateDeltaViolation
--   = IllegalBetChange
--       { attempted :: PlayerHandStateDelta,
--         reason :: String,
--         phase :: HandPhase
--       }
--   | IllegalMutation
--       { attempted :: PlayerHandStateDelta,
--         reason :: String,
--         phase :: HandPhase
--       }
--   | IllegalPhase
--       { attemptedGroup :: [PlayerHandStateDelta],
--         reason :: String,
--         phase :: HandPhase
--       }
--   deriving (Eq, Show)
--
-- legalPlayerHandStateDeltas :: SomePlayerHandFSM -> [PlayerHandStateDelta] -> Either PlayerHandStateDeltaViolation [PlayerHandStateDelta]
-- legalPlayerHandStateDeltas (SomePlayerHandFSM fsm) deltas =
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
--
-- validateInPhase :: HandPhase -> PlayerHandStateDelta -> Either PlayerHandStateDeltaViolation PlayerHandStateDelta
-- validateInPhase phase d = case d of
--   PH.AddCard _ ->
--     Left $ IllegalMutation d "cards are already dealt; cannot add more manually" phase
--   PH.RemoveCard _ ->
--     Left $ IllegalMutation d "card removal is not allowed during play" phase
--   PH.ReplaceCards _ _ ->
--     Left $ IllegalMutation d "card replacement is disallowed after initial deal" phase
--   ReplacePlayerHandIndex _ _ ->
--     Right d
--   PH.ReplaceSplitDepth _ _ ->
--     Right d
--
-- instance Validates PlayerHandState PlayerHandStateDelta PlayerHandStateDeltaViolation where
--   validateDelta delta handState =
--     case handFSMFromState handState of
--       Just (SomePlayerHandFSM fsm) -> validateInFSM fsm delta
--       Nothing ->
--         Left $ IllegalMutation
--           { attempted = delta,
--             reason = "could not infer FSM from state",
--             phase = Decision  -- fallback phase
--           }

-- legalPlayerHandStateDeltas :: SomePlayerHandFSM -> [PlayerHandStateDelta] -> Either PlayerHandStateDeltaViolation [PlayerHandStateDelta]
-- legalPlayerHandStateDeltas (SomePlayerHandFSM fsm) deltas =
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
-- legalPlayerHandStateDeltas ::
--   SomePlayerHandFSM ->
--   StateSnapshot PlayerHandState PlayerHandStateDelta ->
--   (Tick -> Tick) ->
--   [PlayerHandStateDelta] ->
--   Either PlayerHandStateDeltaViolation (StateSnapshot PlayerHandState PlayerHandStateDelta)
-- legalPlayerHandStateDeltas (SomePlayerHandFSM _fsm) snapshot bump =
--   foldl step (Right snapshot)
--   where
--     step acc delta = acc >>= applyIfValid bump delta
-- validateInFSM :: HandFSM p h d s -> PlayerHandStateDelta -> Either PlayerHandStateDeltaViolation PlayerHandStateDelta
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
--       ReplacePlayerHandIndex _ _ -> Right delta
--       ReplaceSplitDepth _ _ -> Right delta
--       _ -> deny "only index/split metadata allowed in decision phase"
--
--     validateDuringPlay = case delta of
--       ReplacePlayerHandIndex _ _ -> Right delta
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
