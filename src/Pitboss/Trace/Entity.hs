{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Trace.Entity (
    Entity (..),
    SomeEntity (..),
    OfferingEntity,
    TableEntity,
    TableShoeEntity,
    TableShoeCursorEntity,
    DealerEntity,
    DealerRoundEntity,
    DealerHandEntity,
    PlayerEntity,
    PlayerSpotEntity,
    PlayerHandEntity,
    mkOfferingEntity,
    mkTableEntity,
    mkTableShoeEntity,
    mkTableShoeCursorEntity,
    mkDealerEntity,
    mkDealerRoundEntity,
    mkDealerHandEntity,
    mkPlayerEntity,
    mkPlayerSpotEntity,
    mkPlayerHandEntity,
    entityKind,
) where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Aeson.Types (Pair, Value)
import Data.Text
import Pitboss.Trace.Entity.Dealer.Types
import Pitboss.Trace.Entity.DealerHand.Types
import Pitboss.Trace.Entity.DealerRound.Types
import Pitboss.Trace.Entity.Offering.Types
import Pitboss.Trace.Entity.Player.Types
import Pitboss.Trace.Entity.PlayerHand.Types
import Pitboss.Trace.Entity.PlayerSpot.Types
import Pitboss.Trace.Entity.Table.Types
import Pitboss.Trace.Entity.TableShoe.Types
import Pitboss.Trace.Entity.TableShoeCursor.Types
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.EntityId

type OfferingEntity = Entity 'Offering
type TableEntity = Entity 'Table
type TableShoeEntity = Entity 'TableShoe
type TableShoeCursorEntity = Entity 'TableShoeCursor
type DealerEntity = Entity 'Dealer
type DealerRoundEntity = Entity 'DealerRound
type DealerHandEntity = Entity 'DealerHand
type PlayerEntity = Entity 'Player
type PlayerSpotEntity = Entity 'PlayerSpot
type PlayerHandEntity = Entity 'PlayerHand

mkOfferingEntity ::
    Meta (ClockedRef OfferingEntityId) ->
    OfferingEntityAttrs ->
    OfferingEntityModes ->
    OfferingEntityRels ->
    OfferingEntity
mkOfferingEntity = OfferingEntity'

mkTableEntity ::
    Meta (ClockedRef TableEntityId) ->
    TableEntityAttrs ->
    TableEntityModes ->
    TableEntityRels ->
    TableEntity
mkTableEntity = TableEntity'

mkTableShoeEntity ::
    Meta (ClockedRef TableShoeEntityId) ->
    TableShoeEntityAttrs ->
    TableShoeEntityModes ->
    TableShoeEntityRels ->
    TableShoeEntity
mkTableShoeEntity = TableShoeEntity'

mkTableShoeCursorEntity ::
    Meta (ClockedRef TableShoeCursorEntityId) ->
    TableShoeCursorEntityAttrs ->
    TableShoeCursorEntityModes ->
    TableShoeCursorEntityRels ->
    TableShoeCursorEntity
mkTableShoeCursorEntity = TableShoeCursorEntity'

mkDealerEntity ::
    Meta (ClockedRef DealerEntityId) ->
    DealerEntityAttrs ->
    DealerEntityModes ->
    DealerEntityRels ->
    DealerEntity
mkDealerEntity = DealerEntity'

mkDealerRoundEntity ::
    Meta (ClockedRef DealerRoundEntityId) ->
    DealerRoundEntityAttrs ->
    DealerRoundEntityModes ->
    DealerRoundEntityRels ->
    DealerRoundEntity
mkDealerRoundEntity = DealerRoundEntity'

mkDealerHandEntity ::
    Meta (ClockedRef DealerHandEntityId) ->
    DealerHandEntityAttrs ->
    DealerHandEntityModes ->
    DealerHandEntityRels ->
    DealerHandEntity
mkDealerHandEntity = DealerHandEntity'

mkPlayerEntity ::
    Meta (ClockedRef PlayerEntityId) ->
    PlayerEntityAttrs ->
    PlayerEntityModes ->
    PlayerEntityRels ->
    PlayerEntity
mkPlayerEntity = PlayerEntity'

mkPlayerSpotEntity ::
    Meta (ClockedRef PlayerSpotEntityId) ->
    PlayerSpotEntityAttrs ->
    PlayerSpotEntityModes ->
    PlayerSpotEntityRels ->
    PlayerSpotEntity
mkPlayerSpotEntity = PlayerSpotEntity'

mkPlayerHandEntity ::
    Meta (ClockedRef PlayerHandEntityId) ->
    PlayerHandEntityAttrs ->
    PlayerHandEntityModes ->
    PlayerHandEntityRels ->
    PlayerHandEntity
mkPlayerHandEntity = PlayerHandEntity'

data Entity k where
    OfferingEntity' ::
        Meta (ClockedRef OfferingEntityId) ->
        OfferingEntityAttrs ->
        OfferingEntityModes ->
        OfferingEntityRels ->
        Entity 'Offering
    TableEntity' ::
        Meta (ClockedRef TableEntityId) ->
        TableEntityAttrs ->
        TableEntityModes ->
        TableEntityRels ->
        Entity 'Table
    TableShoeEntity' ::
        Meta (ClockedRef TableShoeEntityId) ->
        TableShoeEntityAttrs ->
        TableShoeEntityModes ->
        TableShoeEntityRels ->
        Entity 'TableShoe
    TableShoeCursorEntity' ::
        Meta (ClockedRef TableShoeCursorEntityId) ->
        TableShoeCursorEntityAttrs ->
        TableShoeCursorEntityModes ->
        TableShoeCursorEntityRels ->
        Entity 'TableShoeCursor
    PlayerEntity' ::
        Meta (ClockedRef PlayerEntityId) ->
        PlayerEntityAttrs ->
        PlayerEntityModes ->
        PlayerEntityRels ->
        Entity 'Player
    PlayerSpotEntity' ::
        Meta (ClockedRef PlayerSpotEntityId) ->
        PlayerSpotEntityAttrs ->
        PlayerSpotEntityModes ->
        PlayerSpotEntityRels ->
        Entity 'PlayerSpot
    PlayerHandEntity' ::
        Meta (ClockedRef PlayerHandEntityId) ->
        PlayerHandEntityAttrs ->
        PlayerHandEntityModes ->
        PlayerHandEntityRels ->
        Entity 'PlayerHand
    DealerEntity' ::
        Meta (ClockedRef DealerEntityId) ->
        DealerEntityAttrs ->
        DealerEntityModes ->
        DealerEntityRels ->
        Entity 'Dealer
    DealerRoundEntity' ::
        Meta (ClockedRef DealerRoundEntityId) ->
        DealerRoundEntityAttrs ->
        DealerRoundEntityModes ->
        DealerRoundEntityRels ->
        Entity 'DealerRound
    DealerHandEntity' ::
        Meta (ClockedRef DealerHandEntityId) ->
        DealerHandEntityAttrs ->
        DealerHandEntityModes ->
        DealerHandEntityRels ->
        Entity 'DealerHand

deriving instance Show (Entity k)
deriving instance Eq (Entity k)

data SomeEntity where
    SomeEntity :: Entity k -> SomeEntity

instance ToJSON SomeEntity where
    toJSON (SomeEntity e) = case e of
        DealerEntity' m a mo r -> tagged "Dealer" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        DealerHandEntity' m a mo r -> tagged "DealerHand" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        DealerRoundEntity' m a mo r -> tagged "DealerRound" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        OfferingEntity' m a mo r -> tagged "Offering" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        PlayerEntity' m a mo r -> tagged "Player" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        PlayerHandEntity' m a mo r -> tagged "PlayerHand" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        PlayerSpotEntity' m a mo r -> tagged "PlayerSpot" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        TableEntity' m a mo r -> tagged "Table" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        TableShoeEntity' m a mo r -> tagged "TableShoe" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        TableShoeCursorEntity' m a mo r -> tagged "TableShoeCursor" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
      where
        tagged :: Text -> [Pair] -> Value
        tagged tag fields = object ["tag" .= tag, "contents" .= object fields]

instance FromJSON SomeEntity where
    parseJSON = withObject "SomeEntity" $ \o -> do
        tag <- o .: "tag"
        case tag of
            "Dealer" -> SomeEntity <$> (DealerEntity' <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "DealerHand" -> SomeEntity <$> (DealerHandEntity' <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "DealerRound" -> SomeEntity <$> (DealerRoundEntity' <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "Offering" -> SomeEntity <$> (OfferingEntity' <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "Player" -> SomeEntity <$> (PlayerEntity' <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "PlayerHand" -> SomeEntity <$> (PlayerHandEntity' <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "PlayerSpot" -> SomeEntity <$> (PlayerSpotEntity' <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "Table" -> SomeEntity <$> (TableEntity' <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "TableShoe" -> SomeEntity <$> (TableShoeEntity' <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "TableShoeCursor" -> SomeEntity <$> (TableShoeCursorEntity' <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            other -> fail $ "Unknown tag in SomeEntity: " ++ other

entityKind :: Entity k -> EntityKind
entityKind = \case
    DealerEntity'{} -> Dealer
    DealerHandEntity'{} -> DealerHand
    DealerRoundEntity'{} -> DealerRound
    OfferingEntity'{} -> Offering
    PlayerEntity'{} -> Player
    PlayerHandEntity'{} -> PlayerHand
    PlayerSpotEntity'{} -> PlayerSpot
    TableEntity'{} -> Table
    TableShoeEntity'{} -> TableShoe
    TableShoeCursorEntity'{} -> TableShoeCursor
