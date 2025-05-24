{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pitboss.Causality.Trace.Ops where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson.Types
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Text (Text)
import Data.Text qualified as T
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Registry
import Pitboss.Causality.Timeline
import Pitboss.Causality.Trace
import Pitboss.Causality.Trace.Registrable
import Pitboss.Causality.Trace.Types
import Pitboss.Causality.Types.Core

data TraceOp where
    BirthOp :: (EntityIdClass (EntityIdFor k)) => EntityKindWitness k -> EntityIdFor k -> EntityState k -> TraceOp
    MutationOp :: (EntityIdClass (EntityIdFor k)) => EntityKindWitness k -> EntityIdFor k -> SomeDelta k -> TraceOp
    DeathOp :: (EntityIdClass (EntityIdFor k)) => EntityKindWitness k -> EntityIdFor k -> DeathReason k -> TraceOp

instance Eq TraceOp where
    (BirthOp w1 eid1 state1) == (BirthOp w2 eid2 state2) =
        case (w1, w2) of
            (BoutWitness, BoutWitness) ->
                eid1 == eid2 && state1 == state2
            (PlayerWitness, PlayerWitness) ->
                eid1 == eid2 && state1 == state2
            (DealerWitness, DealerWitness) ->
                eid1 == eid2 && state1 == state2
            (RoundWitness, RoundWitness) ->
                eid1 == eid2 && state1 == state2
            (TableWitness, TableWitness) ->
                eid1 == eid2 && state1 == state2
            (ShoeWitness, ShoeWitness) ->
                eid1 == eid2 && state1 == state2
            _ -> False
    (MutationOp w1 eid1 delta1) == (MutationOp w2 eid2 delta2) =
        case (w1, w2) of
            (BoutWitness, BoutWitness) ->
                eid1 == eid2 && delta1 == delta2
            (PlayerWitness, PlayerWitness) ->
                eid1 == eid2 && delta1 == delta2
            (DealerWitness, DealerWitness) ->
                eid1 == eid2 && delta1 == delta2
            (RoundWitness, RoundWitness) ->
                eid1 == eid2 && delta1 == delta2
            (TableWitness, TableWitness) ->
                eid1 == eid2 && delta1 == delta2
            (ShoeWitness, ShoeWitness) ->
                eid1 == eid2 && delta1 == delta2
            _ -> False
    (DeathOp w1 eid1 reason1) == (DeathOp w2 eid2 reason2) =
        case (w1, w2) of
            (BoutWitness, BoutWitness) ->
                eid1 == eid2 && reason1 == reason2
            _ -> False
    _ == _ = False

instance ToJSON TraceOp where
    toJSON (BirthOp witness' eid state) = case witness' of
        BoutWitness -> object ["type" .= String "BirthOp", "kind" .= String "Bout", "entityId" .= eid, "state" .= state]
        RoundWitness -> object ["type" .= String "BirthOp", "kind" .= String "Round", "entityId" .= eid, "state" .= state]
        DealerWitness -> object ["type" .= String "BirthOp", "kind" .= String "Dealer", "entityId" .= eid, "state" .= state]
        PlayerWitness -> object ["type" .= String "BirthOp", "kind" .= String "Player", "entityId" .= eid, "state" .= state]
        TableWitness -> object ["type" .= String "BirthOp", "kind" .= String "Table", "entityId" .= eid, "state" .= state]
        ShoeWitness -> object ["type" .= String "BirthOp", "kind" .= String "Shoe", "entityId" .= eid, "state" .= state]
    toJSON (MutationOp witness' eid delta) = case witness' of
        BoutWitness -> object ["type" .= String "MutationOp", "kind" .= String "Bout", "entityId" .= eid, "delta" .= delta]
        PlayerWitness -> object ["type" .= String "MutationOp", "kind" .= String "Player", "entityId" .= eid, "delta" .= delta]
        _ -> object []
    toJSON (DeathOp witness' eid reason) = case witness' of
        BoutWitness -> object ["type" .= String "DeathOp", "kind" .= String "Bout", "entityId" .= eid, "reason" .= reason]
        _ -> object []

instance FromJSON TraceOp where
    parseJSON = withObject "TraceOp" $ \obj -> do
        opType <- obj .: "type" :: Parser Text
        case opType of
            "BirthOp" -> do
                kind <- obj .: "kind" :: Parser Text
                case kind of
                    "Bout" -> BirthOp BoutWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Dealer" -> BirthOp DealerWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Player" -> BirthOp PlayerWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Round" -> BirthOp RoundWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Shoe" -> BirthOp ShoeWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Table" -> BirthOp TableWitness <$> obj .: "entityId" <*> obj .: "state"
                    _ -> fail $ "Unknown BirthOp kind: " ++ T.unpack kind
            "MutationOp" -> do
                kind <- obj .: "kind" :: Parser Text
                case kind of
                    "Bout" -> MutationOp BoutWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Dealer" -> MutationOp DealerWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Round" -> MutationOp RoundWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Player" -> MutationOp PlayerWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Shoe" -> MutationOp ShoeWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Table" -> MutationOp TableWitness <$> obj .: "entityId" <*> obj .: "delta"
                    _ -> fail $ "Unknown MutationOp kind: " ++ T.unpack kind
            "DeathOp" -> do
                kind <- obj .: "kind" :: Parser Text
                case kind of
                    "Bout" -> DeathOp BoutWitness <$> obj .: "entityId" <*> obj .: "reason"
                    _ -> fail $ "Unknown or unsupported DeathOp kind: " ++ T.unpack kind
            _ -> fail $ "Unknown TraceOp type: " ++ T.unpack opType

applyBirth :: forall k. (Registrable k) => EntityIdFor k -> EntityState k -> Tick -> Trace -> Trace
applyBirth eid state tick trace =
    let timeline = mkTimeline eid tick state
        Registry reg = trace ^. registryLens @k
        updatedReg = Registry $ IHM.insert eid timeline reg
     in trace & registryLens @k .~ updatedReg

applyMutation :: forall k. (Registrable k) => EntityIdFor k -> SomeDelta k -> Tick -> Trace -> Trace
applyMutation eid someDelta tick trace =
    let Registry reg = trace ^. registryLens @k
     in case IHM.lookup eid reg of
            Just timeline ->
                let updatedTimeline =
                        timeline
                            { timelineDeltas = IHM.insertWith (++) tick [someDelta] (timelineDeltas timeline)
                            }
                    updatedReg = Registry $ IHM.insert eid updatedTimeline reg
                 in trace & registryLens @k .~ updatedReg
            Nothing -> trace

applyTraceOp :: TraceOp -> Tick -> Trace -> Trace
applyTraceOp (BirthOp witness' eid state) tick trace =
    case witness' of
        BoutWitness -> applyBirth @'Bout eid state tick trace
        DealerWitness -> applyBirth @'Dealer eid state tick trace
        PlayerWitness -> applyBirth @'Player eid state tick trace
        RoundWitness -> applyBirth @'Round eid state tick trace
        ShoeWitness -> applyBirth @'Shoe eid state tick trace
        TableWitness -> applyBirth @'Table eid state tick trace
applyTraceOp (MutationOp witness' eid someDelta) tick trace =
    case witness' of
        BoutWitness -> applyMutation @'Bout eid someDelta tick trace
        DealerWitness -> applyMutation @'Dealer eid someDelta tick trace
        PlayerWitness -> applyMutation @'Player eid someDelta tick trace
        RoundWitness -> applyMutation @'Round eid someDelta tick trace
        TableWitness -> applyMutation @'Table eid someDelta tick trace
        ShoeWitness -> applyMutation @'Shoe eid someDelta tick trace
applyTraceOp (DeathOp _witness _eid _reason) _tick trace = trace

instance Show TraceOp where
    show (BirthOp witness' eid _state) =
        "BirthOp " ++ show witness' ++ " " ++ show eid ++ " <state>"
    show (MutationOp witness' eid _delta) =
        "MutationOp " ++ show witness' ++ " " ++ show eid ++ " <delta>"
    show (DeathOp witness' eid _reason) =
        "DeathOp " ++ show witness' ++ " " ++ show eid ++ " <reason>"
