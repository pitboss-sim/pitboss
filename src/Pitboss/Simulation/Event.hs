{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Simulation.Event where

import Data.Aeson.Types

import Data.Text qualified as T
import GHC.Generics (Generic)
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Causality.Entity.Witnessable

data BlackjackEvent
    = CardDrawn DealerId ShoeId Card
    | BoutPlayerCardDealt Card BoutId
    | BoutDealerCardDealt Card BoutId
    | BoutPlayerHandSet SomeHand BoutId
    | BoutDealerHandSet SomeHand BoutId
    | BoutPlayerStood BoutId
    | BoutPlayerHit BoutId
    | BoutPlayerDoubledDown BoutId
    | BoutPlayerSplit BoutId
    | BoutPlayerSurrendered BoutId
    | InsuranceSettled [PlayerId]
    | BoutDealerRevealed BoutId
    | BoutDealerHit BoutId
    | BoutDealerStood BoutId
    | BoutSettled BoutId DetailedOutcome
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EconomyEvent
    = BankrollCredit PlayerId Chips
    | BankrollDebit PlayerId Chips
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SomeEntityId where
    SomeEntityId :: (EntityIdClass (EntityIdFor k)) => EntityKindWitness k -> EntityIdFor k -> SomeEntityId

data SomeEntityState where
    SomeEntityState :: EntityKindWitness k -> EntityState k -> SomeEntityState

data SomeDeltaAny where
    SomeDeltaAny :: EntityKindWitness k -> SomeDelta k -> SomeDeltaAny

data SomeDeathReason where
    SomeDeathReason :: EntityKindWitness k -> DeathReason k -> SomeDeathReason

data LifecycleEvent where
    EntityCreated :: (Witnessable k, EntityIdClass (EntityIdFor k)) => EntityKindWitness k -> EntityIdFor k -> EntityState k -> LifecycleEvent
    EntityMutated :: (Witnessable k, EntityIdClass (EntityIdFor k)) => EntityKindWitness k -> EntityIdFor k -> SomeDelta k -> LifecycleEvent
    EntityRetired :: (Witnessable k, EntityIdClass (EntityIdFor k)) => EntityKindWitness k -> EntityIdFor k -> DeathReason k -> LifecycleEvent

instance Eq LifecycleEvent where
    (EntityCreated w1 eid1 state1) == (EntityCreated w2 eid2 state2) =
        case (w1, w2) of
            (BoutWitness, BoutWitness) -> eid1 == eid2 && state1 == state2
            (DealerWitness, DealerWitness) -> eid1 == eid2 && state1 == state2
            (PlayerWitness, PlayerWitness) -> eid1 == eid2 && state1 == state2
            (RoundWitness, RoundWitness) -> eid1 == eid2 && state1 == state2
            (ShoeWitness, ShoeWitness) -> eid1 == eid2 && state1 == state2
            (TableWitness, TableWitness) -> eid1 == eid2 && state1 == state2
            _ -> False
    (EntityMutated w1 eid1 delta1) == (EntityMutated w2 eid2 delta2) =
        case (w1, w2) of
            (BoutWitness, BoutWitness) -> eid1 == eid2 && delta1 == delta2
            (DealerWitness, DealerWitness) -> eid1 == eid2 && delta1 == delta2
            (PlayerWitness, PlayerWitness) -> eid1 == eid2 && delta1 == delta2
            (RoundWitness, RoundWitness) -> eid1 == eid2 && delta1 == delta2
            (ShoeWitness, ShoeWitness) -> eid1 == eid2 && delta1 == delta2
            (TableWitness, TableWitness) -> eid1 == eid2 && delta1 == delta2
            _ -> False
    (EntityRetired w1 eid1 reason1) == (EntityRetired w2 eid2 reason2) =
        case (w1, w2) of
            (BoutWitness, BoutWitness) -> eid1 == eid2 && reason1 == reason2
            _ -> False
    _ == _ = False

instance Eq SomeEntityId where
    (SomeEntityId w1 eid1) == (SomeEntityId w2 eid2) =
        case (w1, w2) of
            (BoutWitness, BoutWitness) -> eid1 == eid2
            (DealerWitness, DealerWitness) -> eid1 == eid2
            (PlayerWitness, PlayerWitness) -> eid1 == eid2
            (RoundWitness, RoundWitness) -> eid1 == eid2
            (ShoeWitness, ShoeWitness) -> eid1 == eid2
            (TableWitness, TableWitness) -> eid1 == eid2
            _ -> False

instance Show SomeEntityId where
    show (SomeEntityId witness' eid) = "SomeEntityId " ++ show witness' ++ " " ++ show eid

instance Eq SomeEntityState where
    (SomeEntityState w1 state1) == (SomeEntityState w2 state2) =
        case (w1, w2) of
            (BoutWitness, BoutWitness) -> state1 == state2
            (DealerWitness, DealerWitness) -> state1 == state2
            (PlayerWitness, PlayerWitness) -> state1 == state2
            (RoundWitness, RoundWitness) -> state1 == state2
            (ShoeWitness, ShoeWitness) -> state1 == state2
            (TableWitness, TableWitness) -> state1 == state2
            _ -> False

instance Show SomeEntityState where
    show (SomeEntityState witness' _state) = "SomeEntityState " ++ show witness' ++ " <state>"

instance Eq SomeDeltaAny where
    (SomeDeltaAny w1 delta1) == (SomeDeltaAny w2 delta2) =
        case (w1, w2) of
            (BoutWitness, BoutWitness) -> delta1 == delta2
            (DealerWitness, DealerWitness) -> delta1 == delta2
            (PlayerWitness, PlayerWitness) -> delta1 == delta2
            (RoundWitness, RoundWitness) -> delta1 == delta2
            (ShoeWitness, ShoeWitness) -> delta1 == delta2
            (TableWitness, TableWitness) -> delta1 == delta2
            _ -> False

instance Show SomeDeltaAny where
    show (SomeDeltaAny witness' _delta) = "SomeDeltaAny " ++ show witness' ++ " <delta>"

instance Eq SomeDeathReason where
    (SomeDeathReason w1 reason1) == (SomeDeathReason w2 reason2) =
        case (w1, w2) of
            (BoutWitness, BoutWitness) -> reason1 == reason2
            _ -> False

instance Show SomeDeathReason where
    show (SomeDeathReason witness' _reason) = "SomeDeathReason " ++ show witness' ++ " <reason>"

instance ToJSON SomeEntityId where
    toJSON (SomeEntityId witness' eid) = case witness' of
        BoutWitness -> object ["kind" .= String "Bout", "entityId" .= eid]
        DealerWitness -> object ["kind" .= String "Dealer", "entityId" .= eid]
        PlayerWitness -> object ["kind" .= String "Player", "entityId" .= eid]
        RoundWitness -> object ["kind" .= String "Round", "entityId" .= eid]
        ShoeWitness -> object ["kind" .= String "Shoe", "entityId" .= eid]
        TableWitness -> object ["kind" .= String "Table", "entityId" .= eid]

instance FromJSON SomeEntityId where
    parseJSON = withObject "SomeEntityId" $ \obj -> do
        kind <- obj .: "kind" :: Parser T.Text
        case kind of
            "Bout" -> SomeEntityId BoutWitness <$> obj .: "entityId"
            "Dealer" -> SomeEntityId DealerWitness <$> obj .: "entityId"
            "Player" -> SomeEntityId PlayerWitness <$> obj .: "entityId"
            "Round" -> SomeEntityId RoundWitness <$> obj .: "entityId"
            "Shoe" -> SomeEntityId ShoeWitness <$> obj .: "entityId"
            "Table" -> SomeEntityId TableWitness <$> obj .: "entityId"
            _ -> fail $ "Unknown SomeEntityId kind: " ++ T.unpack kind

instance ToJSON SomeEntityState where
    toJSON (SomeEntityState witness' state) = case witness' of
        BoutWitness -> object ["kind" .= String "Bout", "state" .= state]
        DealerWitness -> object ["kind" .= String "Dealer", "state" .= state]
        PlayerWitness -> object ["kind" .= String "Player", "state" .= state]
        RoundWitness -> object ["kind" .= String "Round", "state" .= state]
        ShoeWitness -> object ["kind" .= String "Shoe", "state" .= state]
        TableWitness -> object ["kind" .= String "Table", "state" .= state]

instance FromJSON SomeEntityState where
    parseJSON = withObject "SomeEntityState" $ \obj -> do
        kind <- obj .: "kind" :: Parser T.Text
        case kind of
            "Bout" -> SomeEntityState BoutWitness <$> obj .: "state"
            "Dealer" -> SomeEntityState DealerWitness <$> obj .: "state"
            "Player" -> SomeEntityState PlayerWitness <$> obj .: "state"
            "Round" -> SomeEntityState RoundWitness <$> obj .: "state"
            "Shoe" -> SomeEntityState ShoeWitness <$> obj .: "state"
            "Table" -> SomeEntityState TableWitness <$> obj .: "state"
            _ -> fail $ "Unknown SomeEntityState kind: " ++ T.unpack kind

instance ToJSON SomeDeltaAny where
    toJSON (SomeDeltaAny witness' delta) = case witness' of
        BoutWitness -> object ["kind" .= String "Bout", "delta" .= delta]
        DealerWitness -> object ["kind" .= String "Dealer", "delta" .= delta]
        PlayerWitness -> object ["kind" .= String "Player", "delta" .= delta]
        RoundWitness -> object ["kind" .= String "Round", "delta" .= delta]
        ShoeWitness -> object ["kind" .= String "Shoe", "delta" .= delta]
        TableWitness -> object ["kind" .= String "Table", "delta" .= delta]

instance FromJSON SomeDeltaAny where
    parseJSON = withObject "SomeDeltaAny" $ \obj -> do
        kind <- obj .: "kind" :: Parser T.Text
        case kind of
            "Bout" -> SomeDeltaAny BoutWitness <$> obj .: "delta"
            "Dealer" -> SomeDeltaAny DealerWitness <$> obj .: "delta"
            "Player" -> SomeDeltaAny PlayerWitness <$> obj .: "delta"
            "Round" -> SomeDeltaAny RoundWitness <$> obj .: "delta"
            "Shoe" -> SomeDeltaAny ShoeWitness <$> obj .: "delta"
            "Table" -> SomeDeltaAny TableWitness <$> obj .: "delta"
            _ -> fail $ "Unknown SomeDeltaAny kind: " ++ T.unpack kind

instance ToJSON SomeDeathReason where
    toJSON (SomeDeathReason witness' reason) = case witness' of
        BoutWitness -> object ["kind" .= String "Bout", "reason" .= reason]
        _ -> object []

instance FromJSON SomeDeathReason where
    parseJSON = withObject "SomeDeathReason" $ \obj -> do
        kind <- obj .: "kind" :: Parser T.Text
        case kind of
            "Bout" -> SomeDeathReason BoutWitness <$> obj .: "reason"
            _ -> fail $ "Unknown or unsupported SomeDeathReason kind: " ++ T.unpack kind

showEntityId :: EntityKindWitness k -> EntityIdFor k -> String
showEntityId BoutWitness eid = show eid
showEntityId DealerWitness eid = show eid
showEntityId PlayerWitness eid = show eid
showEntityId RoundWitness eid = show eid
showEntityId ShoeWitness eid = show eid
showEntityId TableWitness eid = show eid

instance Show LifecycleEvent where
    show (EntityCreated witness' eid _state) = "EntityCreated " ++ show witness' ++ " " ++ showEntityId witness' eid ++ " <state>"
    show (EntityMutated witness' eid _delta) = "EntityMutated " ++ show witness' ++ " " ++ showEntityId witness' eid ++ " <delta>"
    show (EntityRetired witness' eid _reason) = "EntityRetired " ++ show witness' ++ " " ++ showEntityId witness' eid ++ " <reason>"

data SimulationEvent
    = Game BlackjackEvent
    | Economy EconomyEvent
    | Lifecycle LifecycleEvent
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance ToJSON LifecycleEvent where
    toJSON (EntityCreated witness' eid state) = case witness' of
        BoutWitness -> object ["type" .= String "EntityCreated", "kind" .= String "Bout", "entityId" .= eid, "state" .= state]
        DealerWitness -> object ["type" .= String "EntityCreated", "kind" .= String "Dealer", "entityId" .= eid, "state" .= state]
        PlayerWitness -> object ["type" .= String "EntityCreated", "kind" .= String "Player", "entityId" .= eid, "state" .= state]
        RoundWitness -> object ["type" .= String "EntityCreated", "kind" .= String "Round", "entityId" .= eid, "state" .= state]
        ShoeWitness -> object ["type" .= String "EntityCreated", "kind" .= String "Shoe", "entityId" .= eid, "state" .= state]
        TableWitness -> object ["type" .= String "EntityCreated", "kind" .= String "Table", "entityId" .= eid, "state" .= state]
    toJSON (EntityMutated witness' eid delta) = case witness' of
        BoutWitness -> object ["type" .= String "EntityMutated", "kind" .= String "Bout", "entityId" .= eid, "delta" .= delta]
        DealerWitness -> object ["type" .= String "EntityMutated", "kind" .= String "Dealer", "entityId" .= eid, "delta" .= delta]
        PlayerWitness -> object ["type" .= String "EntityMutated", "kind" .= String "Player", "entityId" .= eid, "delta" .= delta]
        RoundWitness -> object ["type" .= String "EntityMutated", "kind" .= String "Round", "entityId" .= eid, "delta" .= delta]
        ShoeWitness -> object ["type" .= String "EntityMutated", "kind" .= String "Shoe", "entityId" .= eid, "delta" .= delta]
        TableWitness -> object ["type" .= String "EntityMutated", "kind" .= String "Table", "entityId" .= eid, "delta" .= delta]
    toJSON (EntityRetired witness' eid reason) = case witness' of
        BoutWitness -> object ["type" .= String "EntityRetired", "kind" .= String "Bout", "entityId" .= eid, "reason" .= reason]
        _ -> object []

instance FromJSON LifecycleEvent where
    parseJSON = withObject "LifecycleEvent" $ \obj -> do
        typ <- obj .: "type" :: Parser T.Text
        case typ of
            "EntityCreated" -> do
                kind <- obj .: "kind" :: Parser T.Text
                case kind of
                    "Bout" -> EntityCreated BoutWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Dealer" -> EntityCreated DealerWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Player" -> EntityCreated PlayerWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Round" -> EntityCreated RoundWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Shoe" -> EntityCreated ShoeWitness <$> obj .: "entityId" <*> obj .: "state"
                    "Table" -> EntityCreated TableWitness <$> obj .: "entityId" <*> obj .: "state"
                    _ -> fail $ "Unknown EntityCreated kind: " ++ T.unpack kind
            "EntityMutated" -> do
                kind <- obj .: "kind" :: Parser T.Text
                case kind of
                    "Bout" -> EntityMutated BoutWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Dealer" -> EntityMutated DealerWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Player" -> EntityMutated PlayerWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Round" -> EntityMutated RoundWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Shoe" -> EntityMutated ShoeWitness <$> obj .: "entityId" <*> obj .: "delta"
                    "Table" -> EntityMutated TableWitness <$> obj .: "entityId" <*> obj .: "delta"
                    _ -> fail $ "Unknown EntityMutated kind: " ++ T.unpack kind
            "EntityRetired" -> do
                kind <- obj .: "kind" :: Parser T.Text
                case kind of
                    "Bout" -> EntityRetired BoutWitness <$> obj .: "entityId" <*> obj .: "reason"
                    _ -> fail $ "Unknown or unsupported EntityRetired kind: " ++ T.unpack kind
            _ -> fail $ "Unknown LifecycleEvent type: " ++ T.unpack typ

bear :: (Witnessable k, EntityIdClass (EntityIdFor k)) => EntityIdFor k -> EntityState k -> TraceOp
bear eid state = BirthOp (witness state) eid state

mutate :: (Witnessable k, EntityIdClass (EntityIdFor k)) => EntityKindWitness k -> EntityIdFor k -> SomeDelta k -> TraceOp
mutate = MutationOp

bury :: (Witnessable k, EntityIdClass (EntityIdFor k)) => EntityKindWitness k -> EntityIdFor k -> DeathReason k -> TraceOp
bury = DeathOp
