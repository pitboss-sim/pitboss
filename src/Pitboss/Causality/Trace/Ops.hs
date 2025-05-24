{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Causality.Trace.Ops where

import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Entity.Witnessable
import Pitboss.Causality.Registry
import Pitboss.Causality.Timeline
import Pitboss.Causality.Trace
import Pitboss.Causality.Trace.Registrable
import Pitboss.Causality.Trace.Types
import Pitboss.Causality.Types.Core
import Data.Text (Text)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, withObject, (.:), (.=))
import qualified Data.Text as T
import Control.Lens ((^.), (&), (.~))
import Data.Aeson.Types (Parser)

data TraceOp where
    BirthOp :: EntityKindWitness k -> EntityId k -> EntityState k -> TraceOp
    MutationOp :: EntityKindWitness k -> EntityId k -> SomeDelta k -> TraceOp
    DeathOp :: EntityKindWitness k -> EntityId k -> DeathReason k -> TraceOp

instance Eq TraceOp where
    (BirthOp w1 eid1 state1) == (BirthOp w2 eid2 state2) =
        case (w1, w2) of
            (BoutWitness, BoutWitness) ->
                eid1 == eid2 && state1 == state2
            (PlayerWitness, PlayerWitness) ->
                eid1 == eid2 && state1 == state2
            (DealerWitness, DealerWitness) ->
                eid1 == eid2 && state1 == state2
            (PlayerHandWitness, PlayerHandWitness) ->
                eid1 == eid2 && state1 == state2
            (DealerHandWitness, DealerHandWitness) ->
                eid1 == eid2 && state1 == state2
            (PlayerSpotWitness, PlayerSpotWitness) ->
                eid1 == eid2 && state1 == state2
            (DealerRoundWitness, DealerRoundWitness) ->
                eid1 == eid2 && state1 == state2
            (TableWitness, TableWitness) ->
                eid1 == eid2 && state1 == state2
            (TableShoeWitness, TableShoeWitness) ->
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
            (PlayerHandWitness, PlayerHandWitness) ->
                eid1 == eid2 && delta1 == delta2
            (DealerHandWitness, DealerHandWitness) ->
                eid1 == eid2 && delta1 == delta2
            (PlayerSpotWitness, PlayerSpotWitness) ->
                eid1 == eid2 && delta1 == delta2
            (DealerRoundWitness, DealerRoundWitness) ->
                eid1 == eid2 && delta1 == delta2
            (TableWitness, TableWitness) ->
                eid1 == eid2 && delta1 == delta2
            (TableShoeWitness, TableShoeWitness) ->
                eid1 == eid2 && delta1 == delta2
            _ -> False

    (DeathOp w1 eid1 reason1) == (DeathOp w2 eid2 reason2) =
        case (w1, w2) of
            (BoutWitness, BoutWitness) ->
                eid1 == eid2 && reason1 == reason2
            (PlayerHandWitness, PlayerHandWitness) ->
                eid1 == eid2 && reason1 == reason2
            _ -> False

    _ == _ = False

instance ToJSON TraceOp where
    toJSON (BirthOp witness' eid state) = case witness' of
        BoutWitness -> object ["type" .= String "BirthOp", "kind" .= String "Bout", "entityId" .= eid, "state" .= state]
        PlayerWitness -> object ["type" .= String "BirthOp", "kind" .= String "Player", "entityId" .= eid, "state" .= state]
        DealerWitness -> object ["type" .= String "BirthOp", "kind" .= String "Dealer", "entityId" .= eid, "state" .= state]
        PlayerHandWitness -> object ["type" .= String "BirthOp", "kind" .= String "PlayerHand", "entityId" .= eid, "state" .= state]
        DealerHandWitness -> object ["type" .= String "BirthOp", "kind" .= String "DealerHand", "entityId" .= eid, "state" .= state]
        PlayerSpotWitness -> object ["type" .= String "BirthOp", "kind" .= String "PlayerSpot", "entityId" .= eid, "state" .= state]
        DealerRoundWitness -> object ["type" .= String "BirthOp", "kind" .= String "DealerRound", "entityId" .= eid, "state" .= state]
        TableWitness -> object ["type" .= String "BirthOp", "kind" .= String "Table", "entityId" .= eid, "state" .= state]
        TableShoeWitness -> object ["type" .= String "BirthOp", "kind" .= String "TableShoe", "entityId" .= eid, "state" .= state]

    toJSON (MutationOp witness' eid delta) = case witness' of
        BoutWitness -> object ["type" .= String "MutationOp", "kind" .= String "Bout", "entityId" .= eid, "delta" .= delta]
        PlayerWitness -> object ["type" .= String "MutationOp", "kind" .= String "Player", "entityId" .= eid, "delta" .= delta]
        _ -> object []

    toJSON (DeathOp witness' eid reason) = case witness' of
        BoutWitness -> object ["type" .= String "DeathOp", "kind" .= String "Bout", "entityId" .= eid, "reason" .= reason]
        PlayerHandWitness -> object ["type" .= String "DeathOp", "kind" .= String "PlayerHand", "entityId" .= eid, "reason" .= reason]
        _ -> object []

instance FromJSON TraceOp where
    parseJSON = withObject "TraceOp" $ \obj -> do
        opType <- obj .: "type" :: Parser Text
        case opType of
            "BirthOp" -> do
                kind <- obj .: "kind" :: Parser Text
                case kind of
                    "Bout" -> BirthOp BoutWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Player" -> BirthOp PlayerWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Dealer" -> BirthOp DealerWitness <$> obj .: "entityId" <*> obj .: "state"
                    "PlayerHand" -> BirthOp PlayerHandWitness <$> obj .: "entityId" <*> obj .: "state"
                    "DealerHand" -> BirthOp DealerHandWitness <$> obj .: "entityId" <*> obj .: "state"
                    "PlayerSpot" -> BirthOp PlayerSpotWitness <$> obj .: "entityId" <*> obj .: "state"
                    "DealerRound" -> BirthOp DealerRoundWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Table" -> BirthOp TableWitness <$> obj .: "entityId" <*> obj .: "state"
                    "TableShoe" -> BirthOp TableShoeWitness <$> obj .: "entityId" <*> obj .: "state"
                    _ -> fail $ "Unknown BirthOp kind: " ++ T.unpack kind

            "MutationOp" -> do
                kind <- obj .: "kind" :: Parser Text
                case kind of
                    "Bout" -> MutationOp BoutWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Player" -> MutationOp PlayerWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Dealer" -> MutationOp DealerWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "PlayerHand" -> MutationOp PlayerHandWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "DealerHand" -> MutationOp DealerHandWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "PlayerSpot" -> MutationOp PlayerSpotWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "DealerRound" -> MutationOp DealerRoundWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Table" -> MutationOp TableWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "TableShoe" -> MutationOp TableShoeWitness <$> obj .: "entityId" <*> obj .: "delta"
                    _ -> fail $ "Unknown MutationOp kind: " ++ T.unpack kind

            "DeathOp" -> do
                kind <- obj .: "kind" :: Parser Text
                case kind of
                    "Bout" -> DeathOp BoutWitness <$> obj .: "entityId" <*> obj .: "reason"
                    "PlayerHand" -> DeathOp PlayerHandWitness <$> obj .: "entityId" <*> obj .: "reason"
                    _ -> fail $ "Unknown or unsupported DeathOp kind: " ++ T.unpack kind

            _ -> fail $ "Unknown TraceOp type: " ++ T.unpack opType

applyBirthTyped :: forall k. (Registrable k) => EntityId k -> EntityState k -> Tick -> Trace -> Trace
applyBirthTyped eid state tick trace =
    let timeline = mkTimeline eid tick state
        Registry reg = trace ^. registryLens @k
        updatedReg = Registry $ IHM.insert eid timeline reg
     in trace & registryLens @k .~ updatedReg

applyMutationTyped :: forall k. (Registrable k) => EntityId k -> SomeDelta k -> Tick -> Trace -> Trace
applyMutationTyped eid someDelta tick trace =
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
        BoutWitness -> applyBirthTyped @'Bout eid state tick trace
        PlayerWitness -> applyBirthTyped @'Player eid state tick trace
        DealerWitness -> applyBirthTyped @'Dealer eid state tick trace
        PlayerHandWitness -> applyBirthTyped @'PlayerHand eid state tick trace
        DealerHandWitness -> applyBirthTyped @'DealerHand eid state tick trace
        PlayerSpotWitness -> applyBirthTyped @'PlayerSpot eid state tick trace
        DealerRoundWitness -> applyBirthTyped @'DealerRound eid state tick trace
        TableWitness -> applyBirthTyped @'Table eid state tick trace
        TableShoeWitness -> applyBirthTyped @'TableShoe eid state tick trace
applyTraceOp (MutationOp witness' eid someDelta) tick trace =
    case witness' of
        BoutWitness -> applyMutationTyped @'Bout eid someDelta tick trace
        PlayerWitness -> applyMutationTyped @'Player eid someDelta tick trace
        DealerWitness -> applyMutationTyped @'Dealer eid someDelta tick trace
        PlayerHandWitness -> applyMutationTyped @'PlayerHand eid someDelta tick trace
        DealerHandWitness -> applyMutationTyped @'DealerHand eid someDelta tick trace
        PlayerSpotWitness -> applyMutationTyped @'PlayerSpot eid someDelta tick trace
        DealerRoundWitness -> applyMutationTyped @'DealerRound eid someDelta tick trace
        TableWitness -> applyMutationTyped @'Table eid someDelta tick trace
        TableShoeWitness -> applyMutationTyped @'TableShoe eid someDelta tick trace
applyTraceOp (DeathOp _witness _eid _reason) _tick trace = trace -- TODO: implement death handling

instance Show TraceOp where
    show (BirthOp witness' eid _state) =
        "BirthOp " ++ show witness' ++ " " ++ show eid ++ " <state>"
    show (MutationOp witness' eid _delta) =
        "MutationOp " ++ show witness' ++ " " ++ show eid ++ " <delta>"
    show (DeathOp witness' eid _reason) =
        "DeathOp " ++ show witness' ++ " " ++ show eid ++ " <reason>"

createBirth :: (Witnessable k) => EntityId k -> EntityState k -> TraceOp
createBirth eid state = BirthOp (witness state) eid state

createMutation :: (Witnessable k) => EntityId k -> EntityState k -> SomeDelta k -> TraceOp
createMutation eid state = MutationOp (witness state) eid

createDeath :: (Witnessable k) => EntityId k -> EntityState k -> DeathReason k -> TraceOp
createDeath eid state = DeathOp (witness state) eid
