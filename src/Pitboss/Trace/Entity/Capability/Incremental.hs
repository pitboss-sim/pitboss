{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Trace.Entity.Capability.Incremental where

-- import Control.Lens (Lens', lens, (%~), (&))
-- import Data.Map.Strict qualified as Map
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.Id

-- import Pitboss.Trace.Entity.Types.FiniteMap

-- type family AttrsDelta (k :: EntityKind)
-- type family ModesDelta (k :: EntityKind)
-- type family RelsDelta (k :: EntityKind)
--
-- data family Delta (k :: EntityKind) (f :: DeltaTarget)

data family EntityState (k :: EntityKind) (s :: EntityStateSelector)

class Describable delta where
    type Target delta = target | target -> delta

    previewDelta :: delta -> Target delta -> Target delta
    previewDelta = applyDelta

    applyDelta :: delta -> Target delta -> Target delta

    describeDelta :: delta -> Target delta -> String

class (Incremental delta) => Identifiable delta where
    entityToId :: delta -> Target delta -> Uid

class Incremental target where
    type Applicable target = applicable | applicable -> target

    apply :: Applicable target -> target -> target

    describe :: Applicable target -> target -> String

-- instance (Decomposable k, Replaceable k) => Incremental (EntityState k) where
--     type Applicable (EntityState k) = DeltaWrapper k
--
--     apply :: DeltaWrapper k -> EntityState k -> EntityState k
--     apply (DeltaAttrs d) e = replaceAttrs e (apply d (getAttrs e))
--     apply (DeltaModes d) e = replaceModes e (apply d (getModes e))
--     apply (DeltaRels d) e = replaceRels e (apply d (getRels e))
--
--     describe :: DeltaWrapper k -> EntityState k -> String
--     describe (DeltaAttrs d) e = describe d (getAttrs e)
--     describe (DeltaModes d) e = describe d (getModes e)
--     describe (DeltaRels d) e = describe d (getRels e)
--

-- Dealer

-- | Delta wrapper
data family Delta (k :: EntityKind) (s :: EntityStateSelector)

data DeltaWrapper k where
    DeltaAttrs :: Delta k (Part 'Attrs) -> DeltaWrapper k
    DeltaModes :: Delta k (Part 'Modes) -> DeltaWrapper k
    DeltaRels :: Delta k (Part 'Rels) -> DeltaWrapper k

{- | Incremental instance for full entities
instance
    ( Decomposable k
    , Replaceable k
    , HasAttrs k
    , HasModes k
    , HasRels k
    , Incremental (Attrs k)
    , Incremental (Modes k)
    , Incremental (Rels k)
    ) =>
    Incremental (EntityState k)
    where
    type Applicable (EntityState k) = DeltaWrapper k

    apply (DeltaAttrs d) e = e & attrsL %~ apply d
    apply (DeltaModes d) e = e & modesL %~ apply d
    apply (DeltaRels d) e = e & relsL %~ apply d

    describe (DeltaAttrs d) e = describe d (getAttrs e)
    describe (DeltaModes d) e = describe d (getModes e)
    describe (DeltaRels d) e = describe d (getRels e)
-}

-- Table ATTRS

-- instance Incremental ETableAttrs where
--     type Applicable ETableAttrs = Delta 'Table (Part 'Attrs)
--
--     apply (DTableSetName new _) attrs =
--         attrs{_tAttrsName = new}
--     apply (DTableSetMinBet new _) attrs =
--         attrs{_tAttrsMinBet = new}
--     apply (DTableSetOffering new _) attrs =
--         attrs{_tAttrsOfferingUsed = new}
--
--     describe (DTableSetName new old) _ =
--         "Set table name: " ++ show old ++ " → " ++ show new
--     describe (DTableSetMinBet new old) _ =
--         "Set min bet: " ++ show old ++ " → " ++ show new
--     describe (DTableSetOffering new old) _ =
--         "Set offering used: " ++ show old ++ " → " ++ show new
--
-- Table MODES

-- instance Incremental ETableModes where
--     type Applicable ETableModes = Delta 'Table (Part 'Modes)
--
--     apply _ modes = modes
--
--     describe _ _ = "No change to table modes"
--
-- Table RELS

-- instance Incremental ETableRels where
--     type Applicable ETableRels = Delta 'Table (Part 'Rels)
--
--     apply (DTableSetDealer new _) rels =
--         rels{_tRelsManagedByDealer = new}
--
--     describe (DTableSetDealer new old) _ =
--         "Set dealer: " ++ show old ++ " → " ++ show new

-- Table ENTITY (wrapper-based dispatch)

-- instance Incremental (EntityState 'Table) where
--     type Applicable (EntityState 'Table) = DeltaWrapper 'Table
--
--     apply (DeltaAttrs d) e = e & attrsL %~ apply d
--     apply (DeltaModes d) e = e & modesL %~ apply d
--     apply (DeltaRels d) e = e & relsL %~ apply d
--
--     describe (DeltaAttrs d) e = describe d (e ^. attrsL)
--     describe (DeltaModes d) e = describe d (e ^. modesL)
--     describe (DeltaRels d) e = describe d (e ^. relsL)

-- instance Incremental (EntityState k)
--
-- DDealer

-- type instance AttrsDelta 'Dealer = DDealerAttrs
-- type instance ModesDelta 'Dealer = DDealerModes
-- type instance RelsDelta 'Dealer = DDealerRels
--
-- instance Describable (Delta 'Dealer) where
--     type Target (Delta 'Dealer) = EntityState 'Dealer
--
--     applyDelta :: Delta 'Dealer -> EntityState 'Dealer -> EntityState 'Dealer
--     applyDelta delta entity = case delta of
--         DDealerAttrs' delta' -> replaceAttrs entity (applyDelta delta' (getAttrs entity))
--         DDealerModes' delta' -> replaceModes entity (applyDelta delta' (getModes entity))
--         DDealerRels' delta' -> replaceRels entity (applyDelta delta' (getRels entity))
--
--     describeDelta :: Delta 'Dealer -> EntityState 'Dealer -> String
--     describeDelta delta entity = case delta of
--         DDealerAttrs' delta' -> describeDelta delta' (getAttrs entity)
--         DDealerModes' delta' -> describeDelta delta' (getModes entity)
--         DDealerRels' delta' -> describeDelta delta' (getRels entity)
--
-- instance Describable DDealerAttrs where
--     type Target DDealerAttrs = EDealerAttrs
--
--     applyDelta delta target = case delta of
--         DDealerSetName new _ -> target{_dAttrsName = new}
--
--     describeDelta (DDealerSetName new old) _ = "Set dealer name: " ++ show old ++ " -> " ++ show new
--
-- instance Describable DDealerModes where
--     type Target DDealerModes = EDealerModes
--
--     applyDelta delta target = case delta of
--         DDealerSetTableFSM new _ -> target{_dModesDealerTable = new}
--         DDealerSetRoundFSM new _ -> target{_dModesDealerRound = new}
--         DDealerSetHandFSM new _ -> target{_dModesDealerHand = new}
--
--     describeDelta delta _ = case delta of
--         DDealerSetTableFSM new old -> "Set table FSM: " ++ show old ++ " -> " ++ show new
--         DDealerSetRoundFSM new old -> "Set round FSM: " ++ show old ++ " -> " ++ show new
--         DDealerSetHandFSM new old -> "Set hand FSM: " ++ show old ++ " -> " ++ show new
--
-- instance Describable DDealerRels where
--     type Target DDealerRels = EDealerRels
--
--     applyDelta delta target = case delta of
--         DDealerSetActiveRound new _ -> target{_dRelsActiveRound = new}
--         DDealerSetActiveTable new _ -> target{_dRelsActiveTable = new}
--         DDealerSetActiveHand new _ -> target{_dRelsActiveHand = new}
--
--     describeDelta delta _ = case delta of
--         DDealerSetActiveRound new old -> "Set active round: " ++ show old ++ " -> " ++ show new
--         DDealerSetActiveTable new old -> "Set active table: " ++ show old ++ " -> " ++ show new
--         DDealerSetActiveHand new old -> "Set active hand: " ++ show old ++ " -> " ++ show new
--
-- -- DDealerHand
--
-- type instance AttrsDelta 'DealerHand = DDealerHandAttrs
-- type instance ModesDelta 'DealerHand = DDealerHandModes
-- type instance RelsDelta 'DealerHand = DDealerHandRels
--
-- instance Describable (Delta 'DealerHand) where
--     type Target (Delta 'DealerHand) = EntityState 'DealerHand
--
--     applyDelta :: Delta 'DealerHand -> EntityState 'DealerHand -> EntityState 'DealerHand
--     applyDelta delta entity = case delta of
--         DDealerHandAttrs' delta' -> replaceAttrs entity (applyDelta delta' (getAttrs entity))
--         DDealerHandModes' delta' -> replaceModes entity (applyDelta delta' (getModes entity))
--         DDealerHandRels' delta' -> replaceRels entity (applyDelta delta' (getRels entity))
--
--     describeDelta delta entity = case delta of
--         DDealerHandAttrs' delta' -> describeDelta delta' (getAttrs entity)
--         DDealerHandModes' delta' -> describeDelta delta' (getModes entity)
--         DDealerHandRels' delta' -> describeDelta delta' (getRels entity)
--
-- instance Describable DDealerHandAttrs where
--     type Target DDealerHandAttrs = EDealerHandAttrs
--
--     applyDelta = \case
--         DDealerHandPushCard c _ -> \s ->
--             s{_dhAttrsHandCards = c : _dhAttrsHandCards s}
--         DDealerHandPopCard c _ -> \s ->
--             case _dhAttrsHandCards s of
--                 [] -> s
--                 (x : xs)
--                     | x == c -> s{_dhAttrsHandCards = xs}
--                     | otherwise ->
--                         let (before, after) = break (== c) (_dhAttrsHandCards s)
--                          in s{_dhAttrsHandCards = before ++ drop 1 after}
--         DDealerHandSetCards new _ -> \s -> s{_dhAttrsHandCards = new}
--
--     describeDelta :: DDealerHandAttrs -> EDealerHandAttrs -> String
--     describeDelta delta _ = case delta of
--         DDealerHandPushCard c _ -> "Pushed card: " ++ show c
--         DDealerHandPopCard c _ -> "Popped card: " ++ show c
--         DDealerHandSetCards new old -> "Set cards: " ++ show old ++ " -> " ++ show new
--
-- instance Describable DDealerHandModes where
--     type Target DDealerHandModes = EDealerHandModes
--
--     applyDelta :: DDealerHandModes -> EDealerHandModes -> EDealerHandModes
--     applyDelta (DDealerHandSetFSM new _) entity =
--         entity{_dhModesDealerHand = new}
--
--     describeDelta :: DDealerHandModes -> EDealerHandModes -> String
--     describeDelta (DDealerHandSetFSM new old) _ =
--         "Set dealer hand FSM: " ++ show old ++ " -> " ++ show new
--
-- instance Describable DDealerHandRels where
--     type Target DDealerHandRels = EDealerHandRels
--
--     applyDelta :: DDealerHandRels -> EDealerHandRels -> EDealerHandRels
--     applyDelta delta rels = case delta of
--         DDealerHandSetRound new _ -> rels{_dhRelsDealerRound = new}
--         DDealerHandSetDealer new _ -> rels{_dhRelsDealer = new}
--
--     describeDelta :: DDealerHandRels -> EDealerHandRels -> String
--     describeDelta delta _ = case delta of
--         DDealerHandSetRound new old -> "Set dealer hand round: " ++ show old ++ " -> " ++ show new
--         DDealerHandSetDealer new old -> "Set dealer hand dealer: " ++ show old ++ " -> " ++ show new
--
-- -- DDealerRound
--
-- type instance AttrsDelta 'DealerRound = DDealerRoundAttrs
-- type instance ModesDelta 'DealerRound = DDealerRoundModes
-- type instance RelsDelta 'DealerRound = DDealerRoundRels
--
-- instance Describable (Delta 'DealerRound) where
--     type Target (Delta 'DealerRound) = EntityState 'DealerRound
--
--     applyDelta :: Delta 'DealerRound -> EntityState 'DealerRound -> EntityState 'DealerRound
--     applyDelta delta entity = case delta of
--         DDealerRoundAttrs' delta' -> replaceAttrs entity (applyDelta delta' (getAttrs entity))
--         DDealerRoundModes' delta' -> replaceModes entity (applyDelta delta' (getModes entity))
--         DDealerRoundRels' delta' -> replaceRels entity (applyDelta delta' (getRels entity))
--
--     describeDelta :: Delta 'DealerRound -> EntityState 'DealerRound -> String
--     describeDelta delta entity = case delta of
--         DDealerRoundAttrs' delta' -> describeDelta delta' (getAttrs entity)
--         DDealerRoundModes' delta' -> describeDelta delta' (getModes entity)
--         DDealerRoundRels' delta' -> describeDelta delta' (getRels entity)
--
-- instance Describable DDealerRoundAttrs where
--     type Target DDealerRoundAttrs = EDealerRoundAttrs
--
--     applyDelta :: DDealerRoundAttrs -> EDealerRoundAttrs -> EDealerRoundAttrs
--     applyDelta delta target = case delta of
--         DDealerRoundSetNumber new _ -> target{_drAttrsNumber = new}
--
--     describeDelta :: DDealerRoundAttrs -> EDealerRoundAttrs -> String
--     describeDelta (DDealerRoundSetNumber new old) _ =
--         "Set round number: " ++ show old ++ " -> " ++ show new
--
-- instance Describable DDealerRoundModes where
--     type Target DDealerRoundModes = EDealerRoundModes
--
--     applyDelta :: DDealerRoundModes -> EDealerRoundModes -> EDealerRoundModes
--     applyDelta DDealerRoundModes e = e
--
--     describeDelta :: DDealerRoundModes -> EDealerRoundModes -> String
--     describeDelta DDealerRoundModes _ = "No change to dealer round modes"
--
-- instance Describable DDealerRoundRels where
--     type Target DDealerRoundRels = EDealerRoundRels
--
--     applyDelta :: DDealerRoundRels -> EDealerRoundRels -> EDealerRoundRels
--     applyDelta (DDealerRoundSetTableShoe new _) rels = rels{_drRelsTableShoeUsed = new}
--
--     describeDelta :: DDealerRoundRels -> EDealerRoundRels -> String
--     describeDelta (DDealerRoundSetTableShoe new old) _ =
--         "Set round table shoe: " ++ show old ++ " -> " ++ show new
--
-- -- DOffering
--
-- type instance AttrsDelta 'Offering = DOfferingAttrs
-- type instance ModesDelta 'Offering = DOfferingModes
-- type instance RelsDelta 'Offering = DOfferingRels
--
-- instance Describable (Delta 'Offering) where
--     type Target (Delta 'Offering) = EntityState 'Offering
--
--     applyDelta :: Delta 'Offering -> EntityState 'Offering -> EntityState 'Offering
--     applyDelta delta entity = case delta of
--         DOfferingAttrs' delta' -> replaceAttrs entity (applyDelta delta' (getAttrs entity))
--         DOfferingModes' delta' -> replaceModes entity (applyDelta delta' (getModes entity))
--         DOfferingRels' delta' -> replaceRels entity (applyDelta delta' (getRels entity))
--
--     describeDelta :: Delta 'Offering -> EntityState 'Offering -> String
--     describeDelta delta entity = case delta of
--         DOfferingAttrs' delta' -> describeDelta delta' (getAttrs entity)
--         DOfferingModes' _ -> "No change to offering modes"
--         DOfferingRels' _ -> "No change to offering relations"
--
-- instance Describable DOfferingAttrs where
--     type Target DOfferingAttrs = EOfferingAttrs
--
--     applyDelta :: DOfferingAttrs -> EOfferingAttrs -> EOfferingAttrs
--     applyDelta (DOfferingSetOffering new _) _ = EOfferingAttrs new
--
--     describeDelta :: DOfferingAttrs -> EOfferingAttrs -> String
--     describeDelta (DOfferingSetOffering new old) _ =
--         "Set offering: " ++ show old ++ " -> " ++ show new
--
-- instance Describable DOfferingModes where
--     type Target DOfferingModes = EOfferingModes
--
--     applyDelta :: DOfferingModes -> EOfferingModes -> EOfferingModes
--     applyDelta (DOfferingModes _) = id
--
--     describeDelta :: DOfferingModes -> EOfferingModes -> String
--     describeDelta _ _ = "No change to offering modes"
--
-- instance Describable DOfferingRels where
--     type Target DOfferingRels = EOfferingRels
--
--     applyDelta :: DOfferingRels -> EOfferingRels -> EOfferingRels
--     applyDelta _ rels = rels
--
--     describeDelta :: DOfferingRels -> EOfferingRels -> String
--     describeDelta _ _ = "No change to offering relations"
--
-- -- DPlayer
--
-- type instance AttrsDelta 'Player = DPlayerAttrs
-- type instance ModesDelta 'Player = DPlayerModes
-- type instance RelsDelta 'Player = DPlayerRels
--
-- instance Describable (Delta 'Player) where
--     type Target (Delta 'Player) = EntityState 'Player
--
--     applyDelta :: Delta 'Player -> EntityState 'Player -> EntityState 'Player
--     applyDelta delta entity = case delta of
--         DPlayerAttrs' delta' -> replaceAttrs entity (applyDelta delta' (getAttrs entity))
--         DPlayerModes' delta' -> replaceModes entity (applyDelta delta' (getModes entity))
--         DPlayerRels' delta' -> replaceRels entity (applyDelta delta' (getRels entity))
--
--     describeDelta :: Delta 'Player -> EntityState 'Player -> String
--     describeDelta delta entity = case delta of
--         DPlayerAttrs' delta' -> describeDelta delta' (getAttrs entity)
--         DPlayerModes' delta' -> describeDelta delta' (getModes entity)
--         DPlayerRels' delta' -> describeDelta delta' (getRels entity)
--
-- instance Describable DPlayerAttrs where
--     type Target DPlayerAttrs = EPlayerAttrs
--
--     applyDelta :: DPlayerAttrs -> EPlayerAttrs -> EPlayerAttrs
--     applyDelta delta target = case delta of
--         DPlayerSetName new _ -> target{_pAttrsName = new}
--         DPlayerSetBankroll new _ -> target{_pAttrsBankroll = new}
--
--     describeDelta :: DPlayerAttrs -> EPlayerAttrs -> String
--     describeDelta (DPlayerSetName new old) _ =
--         "Set player name: " ++ show old ++ " -> " ++ show new
--     describeDelta (DPlayerSetBankroll new old) _ =
--         "Set player bankroll: " ++ show old ++ " -> " ++ show new
--
-- instance Describable DPlayerModes where
--     type Target DPlayerModes = EPlayerModes
--
--     applyDelta :: DPlayerModes -> EPlayerModes -> EPlayerModes
--     applyDelta DPlayerModes = id
--
--     describeDelta :: DPlayerModes -> EPlayerModes -> String
--     describeDelta _ _ = "No change to player modes"
--
-- instance Describable DPlayerRels where
--     type Target DPlayerRels = EPlayerRels
--
--     applyDelta :: DPlayerRels -> EPlayerRels -> EPlayerRels
--     applyDelta _ r = r
--
--     describeDelta :: DPlayerRels -> EPlayerRels -> String
--     describeDelta _ _ = "No change to player relations"
--
-- -- DPlayerHand
--
-- type instance AttrsDelta 'PlayerHand = DPlayerHandAttrs
-- type instance ModesDelta 'PlayerHand = DPlayerHandModes
-- type instance RelsDelta 'PlayerHand = DPlayerHandRels
--
-- instance Describable (Delta 'PlayerHand) where
--     type Target (Delta 'PlayerHand) = EntityState 'PlayerHand
--
--     applyDelta :: Delta 'PlayerHand -> EntityState 'PlayerHand -> EntityState 'PlayerHand
--     applyDelta delta entity = case delta of
--         DPlayerHandAttrs' delta' -> replaceAttrs entity (applyDelta delta' (getAttrs entity))
--         DPlayerHandModes' delta' -> replaceModes entity (applyDelta delta' (getModes entity))
--         DPlayerHandRels' delta' -> replaceRels entity (applyDelta delta' (getRels entity))
--
--     describeDelta :: Delta 'PlayerHand -> EntityState 'PlayerHand -> String
--     describeDelta delta entity = case delta of
--         DPlayerHandAttrs' delta' -> describeDelta delta' (getAttrs entity)
--         DPlayerHandModes' delta' -> describeDelta delta' (getModes entity)
--         DPlayerHandRels' delta' -> describeDelta delta' (getRels entity)
--
-- instance Describable DPlayerHandAttrs where
--     type Target DPlayerHandAttrs = EPlayerHandAttrs
--
--     applyDelta :: DPlayerHandAttrs -> EPlayerHandAttrs -> EPlayerHandAttrs
--     applyDelta delta target = case delta of
--         DPlayerHandSetPlayerHandIx new _ ->
--             target{_phAttrsHandIx = new}
--         DPlayerHandSetSplitDepth new _ ->
--             target{_phAttrsSplitDepth = new}
--         DPlayerHandPushCard c _ ->
--             target{_phAttrsHandCards = c : _phAttrsHandCards target}
--         DPlayerHandPopCard c _ ->
--             case _phAttrsHandCards target of
--                 [] -> target
--                 (x : xs)
--                     | x == c -> target{_phAttrsHandCards = xs}
--                     | otherwise ->
--                         let (before, after) = break (== c) (_phAttrsHandCards target)
--                          in target{_phAttrsHandCards = before ++ drop 1 after}
--         DPlayerHandSetCards new _ ->
--             target{_phAttrsHandCards = new}
--
--     describeDelta :: DPlayerHandAttrs -> EPlayerHandAttrs -> String
--     describeDelta delta _ = case delta of
--         DPlayerHandSetPlayerHandIx new old -> "Set hand index: " ++ show old ++ " -> " ++ show new
--         DPlayerHandSetSplitDepth new old -> "Set split depth: " ++ show old ++ " -> " ++ show new
--         DPlayerHandPushCard c _ -> "Pushed card: " ++ show c
--         DPlayerHandPopCard c _ -> "Popped card: " ++ show c
--         DPlayerHandSetCards new old -> "Set hand cards: " ++ show old ++ " -> " ++ show new
--
-- instance Describable DPlayerHandModes where
--     type Target DPlayerHandModes = EPlayerHandModes
--
--     applyDelta :: DPlayerHandModes -> EPlayerHandModes -> EPlayerHandModes
--     applyDelta (DPlayerHandSetPlayerHandFSM new _) target = target{_phFsm = new}
--
--     describeDelta :: DPlayerHandModes -> EPlayerHandModes -> String
--     describeDelta (DPlayerHandSetPlayerHandFSM new old) _ =
--         "Set player hand FSM: " ++ show old ++ " -> " ++ show new
--
-- instance Describable DPlayerHandRels where
--     type Target DPlayerHandRels = EPlayerHandRels
--
--     applyDelta :: DPlayerHandRels -> EPlayerHandRels -> EPlayerHandRels
--     applyDelta delta rels = case delta of
--         DPlayerHandSetPlayerSpot new _ -> rels{_phRelsBelongsToPlayerSpot = new}
--
--     describeDelta :: DPlayerHandRels -> EPlayerHandRels -> String
--     describeDelta delta _ = case delta of
--         DPlayerHandSetPlayerSpot new old ->
--             "Set player spot: " ++ show old ++ " -> " ++ show new
--
-- -- DPlayerSpot
--
-- type instance AttrsDelta 'PlayerSpot = DPlayerSpotAttrs
-- type instance ModesDelta 'PlayerSpot = DPlayerSpotModes
-- type instance RelsDelta 'PlayerSpot = DPlayerSpotRels
--
-- instance Describable (Delta 'PlayerSpot) where
--     type Target (Delta 'PlayerSpot) = EntityState 'PlayerSpot
--
--     applyDelta :: Delta 'PlayerSpot -> EntityState 'PlayerSpot -> EntityState 'PlayerSpot
--     applyDelta delta entity = case delta of
--         DPlayerSpotAttrs' delta' -> replaceAttrs entity (applyDelta delta' (getAttrs entity))
--         DPlayerSpotModes' delta' -> replaceModes entity (applyDelta delta' (getModes entity))
--         DPlayerSpotRels' delta' -> replaceRels entity (applyDelta delta' (getRels entity))
--
--     describeDelta :: Delta 'PlayerSpot -> EntityState 'PlayerSpot -> String
--     describeDelta delta entity = case delta of
--         DPlayerSpotAttrs' delta' -> describeDelta delta' (getAttrs entity)
--         DPlayerSpotModes' delta' -> describeDelta delta' (getModes entity)
--         DPlayerSpotRels' delta' -> describeDelta delta' (getRels entity)
--
-- instance Describable DPlayerSpotAttrs where
--     type Target DPlayerSpotAttrs = EPlayerSpotAttrs
--
--     applyDelta :: DPlayerSpotAttrs -> EPlayerSpotAttrs -> EPlayerSpotAttrs
--     applyDelta delta target = case delta of
--         DPlayerSpotSetWager new _ -> target{_psAttrsWager = new}
--
--     describeDelta :: DPlayerSpotAttrs -> EPlayerSpotAttrs -> String
--     describeDelta (DPlayerSpotSetWager new old) _ =
--         "Set spot wager: " ++ show old ++ " -> " ++ show new
--
-- instance Describable DPlayerSpotModes where
--     type Target DPlayerSpotModes = EPlayerSpotModes
--
--     applyDelta :: DPlayerSpotModes -> EPlayerSpotModes -> EPlayerSpotModes
--     applyDelta (DPlayerSpotSetFSM new _) target = target{_psModesPlayerSpot = new}
--
--     describeDelta (DPlayerSpotSetFSM new old) _ =
--         "Set player spot FSM: " ++ show old ++ " -> " ++ show new
--
-- instance Describable DPlayerSpotRels where
--     type Target DPlayerSpotRels = EPlayerSpotRels
--
--     applyDelta :: DPlayerSpotRels -> EPlayerSpotRels -> EPlayerSpotRels
--     applyDelta delta rels = case delta of
--         DPlayerSpotSetPlayer new _ -> rels{_psEntityRelsPlayerId = new}
--         DPlayerSpotSetRound new _ -> rels{_psEntityRelsRoundId = new}
--         DPlayerSpotSetHandOccupancy (_, _) (k, v) ->
--             rels{_psRelsHandOccupancy = insertFiniteMap k v (_psRelsHandOccupancy rels)}
--
--     describeDelta :: DPlayerSpotRels -> EPlayerSpotRels -> String
--     describeDelta (DPlayerSpotSetPlayer new old) _ =
--         "Set player on spot: " ++ show old ++ " -> " ++ show new
--     describeDelta (DPlayerSpotSetRound new old) _ =
--         "Set round on spot: " ++ show old ++ " -> " ++ show new
--     describeDelta (DPlayerSpotSetHandOccupancy (_, _) (ix, _)) _ =
--         "Updated hand occupancy at index: " ++ show ix
--
-- -- DTable
--
-- type instance AttrsDelta 'Table = DTableAttrs
-- type instance ModesDelta 'Table = DTableModes
-- type instance RelsDelta 'Table = DTableRels
--
-- instance Describable (Delta 'Table) where
--     type Target (Delta 'Table) = EntityState 'Table
--
--     applyDelta :: Delta 'Table -> EntityState 'Table -> EntityState 'Table
--     applyDelta delta entity = case delta of
--         DTableAttrs' delta' -> replaceAttrs entity (applyDelta delta' (getAttrs entity))
--         DTableModes' delta' -> replaceModes entity (applyDelta delta' (getModes entity))
--         DTableRels' delta' -> replaceRels entity (applyDelta delta' (getRels entity))
--
--     describeDelta :: Delta 'Table -> EntityState 'Table -> String
--     describeDelta delta entity = case delta of
--         DTableAttrs' delta' -> describeDelta delta' (getAttrs entity)
--         DTableModes' delta' -> describeDelta delta' (getModes entity)
--         DTableRels' delta' -> describeDelta delta' (getRels entity)
--
-- instance Describable DTableAttrs where
--     type Target DTableAttrs = ETableAttrs
--
--     applyDelta :: DTableAttrs -> ETableAttrs -> ETableAttrs
--     applyDelta delta s = case delta of
--         DTableSetName new _ -> s{_tAttrsName = new}
--         DTableSetMinBet new _ -> s{_tAttrsMinBet = new}
--         DTableSetOffering new _ -> s{_tAttrsOfferingUsed = new}
--
--     describeDelta :: DTableAttrs -> ETableAttrs -> String
--     describeDelta (DTableSetName new old) _ =
--         "Set table name: " ++ show old ++ " -> " ++ show new
--     describeDelta (DTableSetMinBet new old) _ =
--         "Set table min bet: " ++ show old ++ " -> " ++ show new
--     describeDelta (DTableSetOffering new old) _ =
--         "Set table offering: " ++ show old ++ " -> " ++ show new
--
-- instance Describable DTableModes where
--     type Target DTableModes = ETableModes
--
--     applyDelta :: DTableModes -> ETableModes -> ETableModes
--     applyDelta (DTableModes _) = id
--
--     describeDelta :: DTableModes -> ETableModes -> String
--     describeDelta _ _ = "No change to table modes"
--
-- instance Describable DTableRels where
--     type Target DTableRels = ETableRels
--
--     applyDelta :: DTableRels -> ETableRels -> ETableRels
--     applyDelta (DTableSetDealer new _) rels = rels{_tRelsManagedByDealer = new}
--
--     describeDelta :: DTableRels -> ETableRels -> String
--     describeDelta (DTableSetDealer new old) _ = "Set dealer: " ++ show old ++ " -> " ++ show new
--
-- -- DTableShoe
--
-- type instance AttrsDelta 'TableShoe = DTableShoeAttrs
-- type instance ModesDelta 'TableShoe = DTableShoeModes
-- type instance RelsDelta 'TableShoe = DTableShoeRels
--
-- instance Describable (Delta 'TableShoe) where
--     type Target (Delta 'TableShoe) = EntityState 'TableShoe
--
--     applyDelta :: Delta 'TableShoe -> EntityState 'TableShoe -> EntityState 'TableShoe
--     applyDelta delta entity = case delta of
--         DTableShoeAttrs' delta' -> replaceAttrs entity (applyDelta delta' (getAttrs entity))
--         DTableShoeModes' delta' -> replaceModes entity (applyDelta delta' (getModes entity))
--         DTableShoeRels' delta' -> replaceRels entity (applyDelta delta' (getRels entity))
--
--     describeDelta :: Delta 'TableShoe -> EntityState 'TableShoe -> String
--     describeDelta delta entity = case delta of
--         DTableShoeAttrs' delta' -> describeDelta delta' (getAttrs entity)
--         DTableShoeModes' delta' -> describeDelta delta' (getModes entity)
--         DTableShoeRels' delta' -> describeDelta delta' (getRels entity)
--
-- instance Describable DTableShoeAttrs where
--     type Target DTableShoeAttrs = ETableShoeAttrs
--
--     applyDelta :: DTableShoeAttrs -> ETableShoeAttrs -> ETableShoeAttrs
--     applyDelta delta attrs = case delta of
--         DTableShoeSetCardStateMap new _ -> attrs{_tsAttrsCardStates = new}
--         DTableShoeSetCardFate ix fate ->
--             attrs{_tsAttrsCardStates = Map.insert ix fate (_tsAttrsCardStates attrs)}
--
--     describeDelta :: DTableShoeAttrs -> ETableShoeAttrs -> String
--     describeDelta (DTableShoeSetCardStateMap _ _) _ =
--         "Set card state map"
--     describeDelta (DTableShoeSetCardFate ix fate) _ =
--         "Set card fate at index " ++ show ix ++ ": " ++ show fate
--
-- instance Describable DTableShoeModes where
--     type Target DTableShoeModes = ETableShoeModes
--
--     applyDelta :: DTableShoeModes -> ETableShoeModes -> ETableShoeModes
--     applyDelta (DTableShoeModes _) = id
--
--     describeDelta :: DTableShoeModes -> ETableShoeModes -> String
--     describeDelta _ _ = "No change to table shoe modes"
--
-- instance Describable DTableShoeRels where
--     type Target DTableShoeRels = ETableShoeRels
--
--     applyDelta :: DTableShoeRels -> ETableShoeRels -> ETableShoeRels
--     applyDelta (DTableShoeSetTable new _) rels =
--         rels{_tsRelsTable = new}
--
--     describeDelta (DTableShoeSetTable new old) _ = "Set table: " ++ show old ++ " -> " ++ show new
--
-- -- helpers
--
-- -- TODO
-- describeSet :: (Show a, Show b) => String -> a -> b -> String
-- describeSet label old new = "Set " ++ label ++ ": " ++ show old ++ " -> " ++ show new
