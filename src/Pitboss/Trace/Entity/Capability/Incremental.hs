{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Trace.Entity.Capability.Incremental where

import Pitboss.Trace.Entity.Capability.Extractable
import Pitboss.Trace.Entity.Capability.Replaceable
import Pitboss.Trace.Entity.Dealer.Delta qualified as D
import Pitboss.Trace.Entity.Dealer.Entity
import Pitboss.Trace.Entity.DealerHand.Delta qualified as DH
import Pitboss.Trace.Entity.DealerHand.Entity
import Pitboss.Trace.Entity.DealerRound.Delta qualified as DR
import Pitboss.Trace.Entity.DealerRound.Entity
import Pitboss.Trace.Entity.Delta
import Pitboss.Trace.Entity.Entity
import Pitboss.Trace.Entity.Offering.Delta
import Pitboss.Trace.Entity.Offering.Delta qualified as O
import Pitboss.Trace.Entity.Offering.Entity
import Pitboss.Trace.Entity.Player.Delta qualified as P
import Pitboss.Trace.Entity.Player.Entity
import Pitboss.Trace.Entity.PlayerHand.Delta qualified as PH
import Pitboss.Trace.Entity.PlayerHand.Entity
import Pitboss.Trace.Entity.PlayerSpot.Delta qualified as PS
import Pitboss.Trace.Entity.PlayerSpot.Entity
import Pitboss.Trace.Entity.Table.Delta
import Pitboss.Trace.Entity.Table.Delta qualified as T
import Pitboss.Trace.Entity.Table.Entity
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.EntityId
import Pitboss.Trace.Entity.Types.FiniteMap

type family AttrsDelta (k :: EntityKind)
type family ModesDelta (k :: EntityKind)
type family RelsDelta (k :: EntityKind)

type instance AttrsDelta 'DealerEntity = D.DealerEntityAttrsDelta
type instance ModesDelta 'DealerEntity = D.DealerEntityModesDelta
type instance RelsDelta 'DealerEntity = D.DealerEntityRelsDelta

type instance AttrsDelta 'DealerHandEntity = DH.DealerHandEntityAttrsDelta
type instance ModesDelta 'DealerHandEntity = DH.DealerHandEntityModesDelta
type instance RelsDelta 'DealerHandEntity = DH.DealerHandEntityRelsDelta

type instance AttrsDelta 'DealerRoundEntity = DR.DealerRoundEntityAttrsDelta
type instance ModesDelta 'DealerRoundEntity = DR.DealerRoundEntityModesDelta
type instance RelsDelta 'DealerRoundEntity = DR.DealerRoundEntityRelsDelta

type instance AttrsDelta 'PlayerEntity = P.PlayerEntityAttrsDelta
type instance ModesDelta 'PlayerEntity = P.PlayerEntityModesDelta
type instance RelsDelta 'PlayerEntity = P.PlayerEntityRelsDelta

type instance AttrsDelta 'PlayerHandEntity = PH.PlayerHandEntityAttrsDelta
type instance ModesDelta 'PlayerHandEntity = PH.PlayerHandEntityModesDelta
type instance RelsDelta 'PlayerHandEntity = PH.PlayerHandEntityRelsDelta

type instance AttrsDelta 'PlayerSpotEntity = PS.PlayerSpotEntityAttrsDelta
type instance ModesDelta 'PlayerSpotEntity = PS.PlayerSpotEntityModesDelta
type instance RelsDelta 'PlayerSpotEntity = PS.PlayerSpotEntityRelsDelta

type instance AttrsDelta 'TableEntity = T.TableEntityAttrsDelta
type instance ModesDelta 'TableEntity = T.TableEntityModesDelta
type instance RelsDelta 'TableEntity = T.TableEntityRelsDelta

type instance AttrsDelta 'OfferingEntity = O.OfferingEntityAttrsDelta
type instance ModesDelta 'OfferingEntity = O.OfferingEntityModesDelta
type instance RelsDelta 'OfferingEntity = O.OfferingEntityRelsDelta

class Incremental delta where
    type Target delta = target | target -> delta

    applyDelta :: delta -> Target delta -> Target delta
    previewDelta :: delta -> Target delta -> Maybe (Target delta)
    describeDelta :: delta -> Target delta -> String

class (Incremental delta) => Identifiable delta where
    entityToId :: delta -> Target delta -> Uid

instance Incremental (Delta 'DealerEntity) where
    type Target (Delta 'DealerEntity) = Entity 'DealerEntity

    applyDelta d e = case d of
        DealerEntityAttrsDelta d' -> replaceAttrs e (applyDelta d' (getAttrs e))
        DealerEntityModesDelta d' -> replaceModes e (applyDelta d' (getModes e))
        DealerEntityRelsDelta d' -> replaceRels e (applyDelta d' (getRels e))

    previewDelta d e = Just (applyDelta d e)

    describeDelta d e = case d of
        DealerEntityAttrsDelta d' -> describeDelta d' (getAttrs e)
        DealerEntityModesDelta d' -> describeDelta d' (getModes e)
        DealerEntityRelsDelta d' -> describeDelta d' (getRels e)

instance Incremental (Delta 'DealerHandEntity) where
    type Target (Delta 'DealerHandEntity) = Entity 'DealerHandEntity

    applyDelta d e = case d of
        DealerHandEntityAttrsDelta d' -> replaceAttrs e (applyDelta d' (getAttrs e))
        DealerHandEntityModesDelta d' -> replaceModes e (applyDelta d' (getModes e))
        DealerHandEntityRelsDelta d' -> replaceRels e (applyDelta d' (getRels e))

    previewDelta d e = Just (applyDelta d e)

    describeDelta d e = case d of
        DealerHandEntityAttrsDelta d' -> describeDelta d' (getAttrs e)
        DealerHandEntityModesDelta d' -> describeDelta d' (getModes e)
        DealerHandEntityRelsDelta d' -> describeDelta d' (getRels e)

instance Incremental (Delta 'DealerRoundEntity) where
    type Target (Delta 'DealerRoundEntity) = Entity 'DealerRoundEntity

    applyDelta d e = case d of
        DealerRoundEntityAttrsDelta d' -> replaceAttrs e (applyDelta d' (getAttrs e))
        DealerRoundEntityModesDelta d' -> replaceModes e (applyDelta d' (getModes e))
        DealerRoundEntityRelsDelta d' -> replaceRels e (applyDelta d' (getRels e))

    previewDelta d e = Just (applyDelta d e)

    describeDelta d e = case d of
        DealerRoundEntityAttrsDelta d' -> describeDelta d' (getAttrs e)
        DealerRoundEntityModesDelta d' -> describeDelta d' (getModes e)
        DealerRoundEntityRelsDelta d' -> describeDelta d' (getRels e)

instance Incremental (Delta 'OfferingEntity) where
    type Target (Delta 'OfferingEntity) = Entity 'OfferingEntity

    applyDelta d e = case d of
        OfferingEntityAttrsDelta d' -> replaceAttrs e (applyDelta d' (getAttrs e))
        OfferingEntityModesDelta d' -> replaceModes e (applyDelta d' (getModes e))
        OfferingEntityRelsDelta d' -> replaceRels e (applyDelta d' (getRels e))

    previewDelta d e = Just (applyDelta d e)

    describeDelta d e = case d of
        OfferingEntityAttrsDelta d' -> describeDelta d' (getAttrs e)
        OfferingEntityModesDelta d' -> describeDelta d' (getModes e)
        OfferingEntityRelsDelta d' -> describeDelta d' (getRels e)

instance Incremental (Delta 'PlayerEntity) where
    type Target (Delta 'PlayerEntity) = Entity 'PlayerEntity

    applyDelta d e = case d of
        PlayerEntityAttrsDelta d' -> replaceAttrs e (applyDelta d' (getAttrs e))
        PlayerEntityModesDelta d' -> replaceModes e (applyDelta d' (getModes e))
        PlayerEntityRelsDelta d' -> replaceRels e (applyDelta d' (getRels e))

    previewDelta d e = Just (applyDelta d e)

    describeDelta d e = case d of
        PlayerEntityAttrsDelta d' -> describeDelta d' (getAttrs e)
        PlayerEntityModesDelta d' -> describeDelta d' (getModes e)
        PlayerEntityRelsDelta d' -> describeDelta d' (getRels e)

instance Incremental (Delta 'PlayerHandEntity) where
    type Target (Delta 'PlayerHandEntity) = Entity 'PlayerHandEntity

    applyDelta d e = case d of
        PlayerHandEntityAttrsDelta d' -> replaceAttrs e (applyDelta d' (getAttrs e))
        PlayerHandEntityModesDelta d' -> replaceModes e (applyDelta d' (getModes e))
        PlayerHandEntityRelsDelta d' -> replaceRels e (applyDelta d' (getRels e))

    previewDelta d e = Just (applyDelta d e)

    describeDelta d e = case d of
        PlayerHandEntityAttrsDelta d' -> describeDelta d' (getAttrs e)
        PlayerHandEntityModesDelta d' -> describeDelta d' (getModes e)
        PlayerHandEntityRelsDelta d' -> describeDelta d' (getRels e)

instance Incremental (Delta 'PlayerSpotEntity) where
    type Target (Delta 'PlayerSpotEntity) = Entity 'PlayerSpotEntity

    applyDelta d e = case d of
        PlayerSpotEntityAttrsDelta d' -> replaceAttrs e (applyDelta d' (getAttrs e))
        PlayerSpotEntityModesDelta d' -> replaceModes e (applyDelta d' (getModes e))
        PlayerSpotEntityRelsDelta d' -> replaceRels e (applyDelta d' (getRels e))

    previewDelta d e = Just (applyDelta d e)

    describeDelta d e = case d of
        PlayerSpotEntityAttrsDelta d' -> describeDelta d' (getAttrs e)
        PlayerSpotEntityModesDelta d' -> describeDelta d' (getModes e)
        PlayerSpotEntityRelsDelta d' -> describeDelta d' (getRels e)

instance Incremental (Delta 'TableEntity) where
    type Target (Delta 'TableEntity) = Entity 'TableEntity

    applyDelta d e = case d of
        TableEntityAttrsDelta d' -> replaceAttrs e (applyDelta d' (getAttrs e))
        TableEntityModesDelta d' -> replaceModes e (applyDelta d' (getModes e))
        TableEntityRelsDelta d' -> replaceRels e (applyDelta d' (getRels e))

    previewDelta d e = Just (applyDelta d e)

    describeDelta d e = case d of
        TableEntityAttrsDelta d' -> describeDelta d' (getAttrs e)
        TableEntityModesDelta d' -> describeDelta d' (getModes e)
        TableEntityRelsDelta d' -> describeDelta d' (getRels e)

-- Note: Delta 'TableShoeEntity is static / Noop only
instance Incremental (Delta 'TableShoeEntity) where
    type Target (Delta 'TableShoeEntity) = Entity 'TableShoeEntity
    applyDelta Noop e = e
    previewDelta Noop = Just
    describeDelta Noop _ = "No change"

instance Incremental OfferingEntityAttrsDelta where
    type Target OfferingEntityAttrsDelta = OfferingEntityAttrs

    applyDelta :: OfferingEntityAttrsDelta -> OfferingEntityAttrs -> OfferingEntityAttrs
    applyDelta (ReplaceOffering _ new) state =
        state{_offeringEntityAttrsOffering = new}

    previewDelta :: OfferingEntityAttrsDelta -> OfferingEntityAttrs -> Maybe OfferingEntityAttrs
    previewDelta (ReplaceOffering old _) state =
        if _offeringEntityAttrsOffering state == old
            then Just (applyDelta (ReplaceOffering old old) state)
            else Nothing

    describeDelta :: OfferingEntityAttrsDelta -> OfferingEntityAttrs -> String
    describeDelta _ _ = "Replaced offering (details omitted)"

instance Incremental OfferingEntityModesDelta where
    type Target OfferingEntityModesDelta = OfferingEntityModes

    applyDelta :: OfferingEntityModesDelta -> OfferingEntityModes -> OfferingEntityModes
    applyDelta O.NoopModes e = e

    previewDelta :: OfferingEntityModesDelta -> OfferingEntityModes -> Maybe OfferingEntityModes
    previewDelta O.NoopModes = Just

    describeDelta :: OfferingEntityModesDelta -> OfferingEntityModes -> String
    describeDelta O.NoopModes _ = "Noop FSM delta (Offering)"

instance Incremental OfferingEntityRelsDelta where
    type Target OfferingEntityRelsDelta = OfferingEntityRels

    applyDelta :: OfferingEntityRelsDelta -> OfferingEntityRels -> OfferingEntityRels
    applyDelta delta rels = case delta of
        AddTable tid -> rels{_offeringEntityRelsAssociatedTables = tid : _offeringEntityRelsAssociatedTables rels}
        RemoveTable tid -> rels{_offeringEntityRelsAssociatedTables = filter (/= tid) (_offeringEntityRelsAssociatedTables rels)}

    previewDelta :: OfferingEntityRelsDelta -> OfferingEntityRels -> Maybe OfferingEntityRels
    previewDelta delta rels = case delta of
        AddTable tid ->
            if tid `notElem` _offeringEntityRelsAssociatedTables rels
                then Just $ applyDelta delta rels
                else Nothing
        RemoveTable tid ->
            if tid `elem` _offeringEntityRelsAssociatedTables rels
                then Just $ applyDelta delta rels
                else Nothing

    describeDelta :: OfferingEntityRelsDelta -> OfferingEntityRels -> String
    describeDelta delta _ = case delta of
        AddTable tid -> "Added table to offering: " ++ show tid
        RemoveTable tid -> "Removed table from offering: " ++ show tid

instance Incremental TableEntityAttrsDelta where
    type Target TableEntityAttrsDelta = TableEntityAttrs

    applyDelta delta s = case delta of
        SetTableName _ new -> s{_tableEntityAttrsName = new}
        SetMinBet _ new -> s{_tableEntityAttrsMinBet = new}
        SetOffering _ new -> s{_tableEntityAttrsOfferingUsed = new}
        StartRound _ new -> s{_tableEntityAttrsCurrentRound = Just new}
        EndRound _ -> s{_tableEntityAttrsCurrentRound = Nothing}

    previewDelta delta entity = case delta of
        SetTableName old _ | old == _tableEntityAttrsName entity -> Just (applyDelta delta entity)
        SetMinBet old _ | old == _tableEntityAttrsMinBet entity -> Just (applyDelta delta entity)
        SetOffering old _ | old == _tableEntityAttrsOfferingUsed entity -> Just (applyDelta delta entity)
        StartRound prev _ | _tableEntityAttrsCurrentRound entity == prev -> Just (applyDelta delta entity)
        EndRound old | _tableEntityAttrsCurrentRound entity == Just old -> Just (applyDelta delta entity)
        _ -> Nothing

    describeDelta delta _ = case delta of
        SetTableName old new -> "Changed table name: " ++ old ++ " -> " ++ new
        SetMinBet old new -> "Changed minimum bet: " ++ show old ++ " -> " ++ show new
        SetOffering old new -> "Changed offering: " ++ show old ++ " -> " ++ show new
        StartRound prev new -> "Started round: " ++ show new ++ maybe "" (\p -> " (previous: " ++ show p ++ ")") prev
        EndRound old -> "Ended round: " ++ show old

instance Incremental TableEntityModesDelta where
    type Target TableEntityModesDelta = TableEntityModes
    applyDelta T.NoopModes e = e
    previewDelta T.NoopModes = Just
    describeDelta T.NoopModes _ = "Noop FSM delta"

instance Incremental TableEntityRelsDelta where
    type Target TableEntityRelsDelta = TableEntityRels

    applyDelta delta rels = case delta of
        AssignDealer _ new -> rels{_tableEntityRelsManagedByDealer = pure new}
        UnassignDealer _ -> rels{_tableEntityRelsManagedByDealer = Nothing}

    previewDelta delta entity = case delta of
        AssignDealer old _ | _tableEntityRelsManagedByDealer entity == old -> Just (applyDelta delta entity)
        UnassignDealer old -> case _tableEntityRelsManagedByDealer entity of
            Just old' | old == old' -> Just (applyDelta delta entity)
            _ -> Nothing
        _ -> Nothing

    describeDelta delta _ = case delta of
        AssignDealer old new -> "Assigned dealer: " ++ maybe "None" show old ++ " -> " ++ show new
        UnassignDealer old -> "Unassigned dealer: " ++ show old

instance Incremental DR.DealerRoundEntityAttrsDelta where
    type Target DR.DealerRoundEntityAttrsDelta = DealerRoundEntityAttrs

    applyDelta :: DR.DealerRoundEntityAttrsDelta -> DealerRoundEntityAttrs -> DealerRoundEntityAttrs
    applyDelta delta state = case delta of
        DR.SetDealerRoundEntityNumber n -> state{_dealerRoundEntityAttrsNumber = n}
        DR.SetActive b -> state{_dealerRoundEntityAttrsIsActive = b}

    previewDelta :: DR.DealerRoundEntityAttrsDelta -> DealerRoundEntityAttrs -> Maybe DealerRoundEntityAttrs
    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta :: DR.DealerRoundEntityAttrsDelta -> DealerRoundEntityAttrs -> String
    describeDelta delta _ = case delta of
        DR.SetDealerRoundEntityNumber n -> "Set round number to " ++ show n
        DR.SetActive b -> "Set active status to " ++ show b

instance Incremental DR.DealerRoundEntityModesDelta where
    type Target DR.DealerRoundEntityModesDelta = DealerRoundEntityModes

    applyDelta :: DR.DealerRoundEntityModesDelta -> DealerRoundEntityModes -> DealerRoundEntityModes
    applyDelta DR.NoopModes e = e

    previewDelta :: DR.DealerRoundEntityModesDelta -> DealerRoundEntityModes -> Maybe DealerRoundEntityModes
    previewDelta DR.NoopModes = Just

    describeDelta :: DR.DealerRoundEntityModesDelta -> DealerRoundEntityModes -> String
    describeDelta DR.NoopModes _ = "Noop FSM delta (DealerRound)"

instance Incremental DR.DealerRoundEntityRelsDelta where
    type Target DR.DealerRoundEntityRelsDelta = DealerRoundEntityRels

    applyDelta :: DR.DealerRoundEntityRelsDelta -> DealerRoundEntityRels -> DealerRoundEntityRels
    applyDelta delta rels = case delta of
        DR.SetTableShoeUsed shoe -> rels{_dealerRoundEntityRelsTableShoeUsed = shoe}

    previewDelta :: DR.DealerRoundEntityRelsDelta -> DealerRoundEntityRels -> Maybe DealerRoundEntityRels
    previewDelta delta rels = Just $ applyDelta delta rels

    describeDelta :: DR.DealerRoundEntityRelsDelta -> DealerRoundEntityRels -> String
    describeDelta (DR.SetTableShoeUsed new) _ = "Updated shoe used to " ++ show new

instance Incremental D.DealerEntityAttrsDelta where
    type Target D.DealerEntityAttrsDelta = DealerEntityAttrs

    applyDelta :: D.DealerEntityAttrsDelta -> DealerEntityAttrs -> DealerEntityAttrs
    applyDelta delta state = case delta of
        D.RenameDealer _ new -> state{_dealerEntityAttrsName = new}
        D.ReplaceAssignedTable _ new -> state{_dealerEntityAttrsAssignedTable = new}

    previewDelta :: D.DealerEntityAttrsDelta -> DealerEntityAttrs -> Maybe DealerEntityAttrs
    previewDelta delta state = case delta of
        D.RenameDealer _ _ -> Just $ applyDelta delta state
        D.ReplaceAssignedTable old _ ->
            if _dealerEntityAttrsAssignedTable state == old
                then Just $ applyDelta delta state
                else Nothing

    describeDelta :: D.DealerEntityAttrsDelta -> DealerEntityAttrs -> String
    describeDelta delta _ = case delta of
        D.RenameDealer old new ->
            "Renamed dealer: " ++ old ++ " -> " ++ new
        D.ReplaceAssignedTable old new ->
            "Reassigned dealer table: " ++ show old ++ " -> " ++ show new

instance Incremental D.DealerEntityModesDelta where
    type Target D.DealerEntityModesDelta = DealerEntityModes

    applyDelta delta entity =
        let (ft, fr, fh) = (_dealerEntityModesDealerTable entity, _dealerEntityModesDealerRound entity, _dealerEntityModesDealerHand entity)
            (ft', fr', fh') = case delta of
                D.ReplaceTableFSM _ new -> (new, fr, fh)
                D.ReplaceRoundFSM _ new -> (ft, new, fh)
                D.ReplaceHandFSM _ new -> (ft, fr, new)
         in entity
                { _dealerEntityModesDealerTable = ft'
                , _dealerEntityModesDealerRound = fr'
                , _dealerEntityModesDealerHand = fh'
                }

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta :: D.DealerEntityModesDelta -> DealerEntityModes -> String
    describeDelta delta _ = case delta of
        D.ReplaceTableFSM _ new -> "Replaced DealerTableFSM with " ++ show new
        D.ReplaceRoundFSM _ new -> "Replaced DealerRoundFSM with " ++ show new
        D.ReplaceHandFSM _ new -> "Replaced DealerHandFSM with " ++ show new

instance Incremental D.DealerEntityRelsDelta where
    type Target D.DealerEntityRelsDelta = DealerEntityRels

    applyDelta delta rels = case delta of
        D.UpdateRound _ new -> rels{_dealerEntityRelsCurrentRound = new}
        D.UpdateHand _ new -> rels{_dealerEntityRelsActiveHand = new}

    previewDelta delta state = case delta of
        D.UpdateRound old _ ->
            if _dealerEntityRelsCurrentRound state == old
                then Just $ applyDelta delta state
                else Nothing
        D.UpdateHand old _ ->
            if _dealerEntityRelsActiveHand state == old
                then Just $ applyDelta delta state
                else Nothing

    describeDelta delta _ = case delta of
        D.UpdateRound old new ->
            "Updated dealer round: " ++ show old ++ " -> " ++ show new
        D.UpdateHand old new ->
            "Updated dealer hand: " ++ show old ++ " -> " ++ show new

instance Incremental DH.DealerHandEntityAttrsDelta where
    type Target DH.DealerHandEntityAttrsDelta = DealerHandEntityAttrs

    applyDelta = \case
        DH.AddCard c -> \s -> s{_dealerHandEntityAttrsHandCards = c : _dealerHandEntityAttrsHandCards s}
        DH.RemoveCard c -> \s -> s{_dealerHandEntityAttrsHandCards = filter (/= c) (_dealerHandEntityAttrsHandCards s)}
        DH.ReplaceCards _ new -> \s -> s{_dealerHandEntityAttrsHandCards = new}

    previewDelta d s = Just $ applyDelta d s

    describeDelta d _ = case d of
        DH.AddCard c -> "Added card: " ++ show c
        DH.RemoveCard c -> "Removed card: " ++ show c
        DH.ReplaceCards old new -> "Replaced cards: " ++ show old ++ " -> " ++ show new

instance Incremental DH.DealerHandEntityModesDelta where
    type Target DH.DealerHandEntityModesDelta = DealerHandEntityModes

    applyDelta (DH.ReplaceFSM _ new) entity =
        entity{_dealerHandEntityModesDealerHand = new}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta (DH.ReplaceFSM _ new) _ =
        "Replaced DealerHand FSM with: " ++ show new

instance Incremental DH.DealerHandEntityRelsDelta where
    type Target DH.DealerHandEntityRelsDelta = DealerHandEntityRels

    applyDelta delta rels = case delta of
        DH.UpdatePlayerSpot _ new ->
            rels{_dealerHandEntityRelsBelongsToPlayerSpot = new}
        DH.UpdateDealerRound _ new ->
            rels{_dealerHandEntityRelsBelongsToDealerRound = new}
        DH.UpdateDealer _ new ->
            rels{_dealerHandEntityRelsOwnedByDealer = new}

    previewDelta delta rels = case delta of
        DH.UpdatePlayerSpot old _ ->
            if _dealerHandEntityRelsBelongsToPlayerSpot rels == old
                then Just $ applyDelta delta rels
                else Nothing
        DH.UpdateDealerRound old _ ->
            if _dealerHandEntityRelsBelongsToDealerRound rels == old
                then Just $ applyDelta delta rels
                else Nothing
        DH.UpdateDealer old _ ->
            if _dealerHandEntityRelsOwnedByDealer rels == old
                then Just $ applyDelta delta rels
                else Nothing

    describeDelta delta _ = case delta of
        DH.UpdatePlayerSpot old new ->
            "Updated PlayerSpot ref: " ++ show old ++ " -> " ++ show new
        DH.UpdateDealerRound old new ->
            "Updated DealerRound ref: " ++ show old ++ " -> " ++ show new
        DH.UpdateDealer old new ->
            "Updated Dealer ref: " ++ show old ++ " -> " ++ show new

instance Incremental P.PlayerEntityAttrsDelta where
    type Target P.PlayerEntityAttrsDelta = PlayerEntityAttrs

    applyDelta :: P.PlayerEntityAttrsDelta -> PlayerEntityAttrs -> PlayerEntityAttrs
    applyDelta delta state = case delta of
        P.RenamePlayer _ new -> state{_playerEntityAttrsPlayerName = new}
        P.SetBankroll _ new -> state{_playerEntityAttrsBankroll = new}

    previewDelta :: P.PlayerEntityAttrsDelta -> PlayerEntityAttrs -> Maybe PlayerEntityAttrs
    previewDelta delta state = Just $ applyDelta delta state

    describeDelta :: P.PlayerEntityAttrsDelta -> PlayerEntityAttrs -> String
    describeDelta delta _ = case delta of
        P.RenamePlayer old new -> "Renamed player: " ++ old ++ " -> " ++ new
        P.SetBankroll old new -> "Updated bankroll: " ++ show old ++ " -> " ++ show new

instance Incremental P.PlayerEntityModesDelta where
    type Target P.PlayerEntityModesDelta = PlayerEntityModes

    applyDelta :: P.PlayerEntityModesDelta -> PlayerEntityModes -> PlayerEntityModes
    applyDelta P.NoopModes m = m

    previewDelta :: P.PlayerEntityModesDelta -> PlayerEntityModes -> Maybe PlayerEntityModes
    previewDelta P.NoopModes = Just

    describeDelta :: P.PlayerEntityModesDelta -> PlayerEntityModes -> String
    describeDelta P.NoopModes _ = "Noop FSM delta (Player)"

instance Incremental P.PlayerEntityRelsDelta where
    type Target P.PlayerEntityRelsDelta = PlayerEntityRels

    applyDelta :: P.PlayerEntityRelsDelta -> PlayerEntityRels -> PlayerEntityRels
    applyDelta delta rels = case delta of
        P.UpdateCloneOf _ new -> rels{_playerEntityRelsClonedFrom = new}
        P.UpdateSeatedAt _ new -> rels{_playerEntityRelsSeatedAt = new}

    previewDelta :: P.PlayerEntityRelsDelta -> PlayerEntityRels -> Maybe PlayerEntityRels
    previewDelta delta rels = case delta of
        P.UpdateCloneOf old _ | _playerEntityRelsClonedFrom rels == old -> Just $ applyDelta delta rels
        P.UpdateSeatedAt old _ | _playerEntityRelsSeatedAt rels == old -> Just $ applyDelta delta rels
        _ -> Nothing

    describeDelta :: P.PlayerEntityRelsDelta -> PlayerEntityRels -> String
    describeDelta delta _ = case delta of
        P.UpdateCloneOf old new ->
            "Updated cloned-from: " ++ show old ++ " -> " ++ show new
        P.UpdateSeatedAt old new ->
            "Updated table seat: " ++ show old ++ " -> " ++ show new

instance Incremental PS.PlayerSpotEntityAttrsDelta where
    type Target PS.PlayerSpotEntityAttrsDelta = PlayerSpotEntityAttrs

    applyDelta delta state = case delta of
        PS.ReplaceWager _ new -> state{_playerSpotEntityAttrsWager = new}

    previewDelta delta state = case delta of
        PS.ReplaceWager old _ | _playerSpotEntityAttrsWager state == old -> Just (applyDelta delta state)
        _ -> Nothing

    describeDelta (PS.ReplaceWager old new) _ =
        "Replaced wager: " ++ show old ++ " -> " ++ show new

instance Incremental PS.PlayerSpotEntityModesDelta where
    type Target PS.PlayerSpotEntityModesDelta = PlayerSpotEntityModes

    applyDelta (PS.ReplaceFSM _ new) state = state{_playerSpotEntityModesPlayerSpot = new}

    previewDelta delta state = Just (applyDelta delta state)

    describeDelta (PS.ReplaceFSM _ new) _ =
        "Replaced PlayerSpot FSM with: " ++ show new

instance Incremental PS.PlayerSpotEntityRelsDelta where
    type Target PS.PlayerSpotEntityRelsDelta = PlayerSpotEntityRels

    applyDelta delta rels = case delta of
        PS.UpdatePlayer _ new -> rels{_playerSpotEntityRelsPlayerEntityId = new}
        PS.UpdateRound _ new -> rels{_playerSpotEntityRelsRoundEntityId = new}
        PS.UpdateHandOccupancy (_, _) (k, v) ->
            rels{_playerSpotEntityRelsHandOccupancy = insertFiniteMap k v (_playerSpotEntityRelsHandOccupancy rels)}

    previewDelta delta rels =
        case delta of
            PS.UpdatePlayer old _
                | _playerSpotEntityRelsPlayerEntityId rels == old ->
                    Just (applyDelta delta rels)
            PS.UpdateRound old _
                | _playerSpotEntityRelsRoundEntityId rels == old ->
                    Just (applyDelta delta rels)
            PS.UpdateHandOccupancy (oldK, oldV) _
                | Just oldV == lookupFiniteMap oldK (_playerSpotEntityRelsHandOccupancy rels) ->
                    Just (applyDelta delta rels)
            _ -> Nothing

    describeDelta delta _ = case delta of
        PS.UpdatePlayer old new ->
            "Updated player ID: " ++ show old ++ " -> " ++ show new
        PS.UpdateRound old new ->
            "Updated round ID: " ++ show old ++ " -> " ++ show new
        PS.UpdateHandOccupancy (k1, v1) (k2, v2) ->
            "Updated hand occupancy: " ++ show k1 ++ "=" ++ show v1 ++ " -> " ++ show k2 ++ "=" ++ show v2

instance Incremental PH.PlayerHandEntityAttrsDelta where
    type Target PH.PlayerHandEntityAttrsDelta = PlayerHandEntityAttrs

    applyDelta delta state = case delta of
        PH.AddCard c -> state{_playerHandEntityAttrsHandCards = _playerHandEntityAttrsHandCards state ++ [c]}
        PH.RemoveCard c -> state{_playerHandEntityAttrsHandCards = filter (/= c) (_playerHandEntityAttrsHandCards state)}
        PH.ReplaceCards _ new -> state{_playerHandEntityAttrsHandCards = new}
        PH.ReplacePlayerHandIndex _ new -> state{_playerHandEntityAttrsHandIx = new}
        PH.ReplaceSplitDepth _ new -> state{_playerHandEntityAttrsSplitDepth = new}

    previewDelta delta state = case delta of
        PH.ReplacePlayerHandIndex old _ | old == _playerHandEntityAttrsHandIx state -> Just (applyDelta delta state)
        PH.ReplaceSplitDepth old _ | old == _playerHandEntityAttrsSplitDepth state -> Just (applyDelta delta state)
        _ -> Just (applyDelta delta state)

    describeDelta delta _ = case delta of
        PH.AddCard c -> "Added card: " ++ show c
        PH.RemoveCard c -> "Removed card: " ++ show c
        PH.ReplaceCards old new -> "Replaced cards: " ++ show old ++ " -> " ++ show new
        PH.ReplacePlayerHandIndex old new -> "Changed hand index: " ++ show old ++ " -> " ++ show new
        PH.ReplaceSplitDepth old new -> "Changed split depth: " ++ show old ++ " -> " ++ show new

instance Incremental PH.PlayerHandEntityModesDelta where
    type Target PH.PlayerHandEntityModesDelta = PlayerHandEntityModes

    applyDelta (PH.ReplaceFSM _ new) state = state{_playerHandEntityFsm = new}

    previewDelta delta state = Just $ applyDelta delta state

    describeDelta (PH.ReplaceFSM _ new) _ = "Replaced PlayerHand FSM with: " ++ show new

instance Incremental PH.PlayerHandEntityRelsDelta where
    type Target PH.PlayerHandEntityRelsDelta = PlayerHandEntityRels

    applyDelta delta rels = case delta of
        PH.UpdatePlayerSpot _ new -> rels{_playerHandEntityRelsBelongsToPlayerSpot = new}
        PH.UpdateDealerRound _ new -> rels{_playerHandEntityRelsBelongsToDealerRound = new}
        PH.UpdatePlayer _ new -> rels{_playerHandEntityRelsOwnedByPlayer = new}

    previewDelta delta rels = case delta of
        PH.UpdatePlayerSpot old _ | old == _playerHandEntityRelsBelongsToPlayerSpot rels -> Just $ applyDelta delta rels
        PH.UpdateDealerRound old _ | old == _playerHandEntityRelsBelongsToDealerRound rels -> Just $ applyDelta delta rels
        PH.UpdatePlayer old _ | old == _playerHandEntityRelsOwnedByPlayer rels -> Just $ applyDelta delta rels
        _ -> Nothing

    describeDelta delta _ = case delta of
        PH.UpdatePlayerSpot old new -> "Changed spot: " ++ show old ++ " -> " ++ show new
        PH.UpdateDealerRound old new -> "Changed round: " ++ show old ++ " -> " ++ show new
        PH.UpdatePlayer old new -> "Changed player: " ++ show old ++ " -> " ++ show new
