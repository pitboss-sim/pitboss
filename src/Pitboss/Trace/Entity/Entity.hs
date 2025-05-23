{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Trace.Entity.Entity (
    module Pitboss.Trace.Entity.Dealer.Entity,
    module Pitboss.Trace.Entity.DealerHand.Entity,
    module Pitboss.Trace.Entity.DealerRound.Entity,
    module Pitboss.Trace.Entity.Offering.Entity,
    module Pitboss.Trace.Entity.Player.Entity,
    module Pitboss.Trace.Entity.PlayerHand.Entity,
    module Pitboss.Trace.Entity.PlayerSpot.Entity,
    module Pitboss.Trace.Entity.Table.Entity,
    module Pitboss.Trace.Entity.TableShoe.Entity,
    module Pitboss.Trace.Entity.Types,
    module Pitboss.Trace.Entity.Types.EntityId,
    Entity (..),
    ESome (..),
    EOffering,
    ETable,
    ETableShoe,
    EDealer,
    EDealerRound,
    EDealerHand,
    EPlayer,
    EPlayerSpot,
    EPlayerHand,
    mkOffering,
    mkTable,
    mkTableShoe,
    mkDealer,
    mkDealerRound,
    mkDealerHand,
    mkPlayer,
    mkPlayerSpot,
    mkPlayerHand,
    entityKind,
) where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Aeson.Types (Pair, Value)
import Data.Text
import Pitboss.Trace.Entity.Dealer.Entity
import Pitboss.Trace.Entity.DealerHand.Entity
import Pitboss.Trace.Entity.DealerRound.Entity
import Pitboss.Trace.Entity.Offering.Entity
import Pitboss.Trace.Entity.Player.Entity
import Pitboss.Trace.Entity.PlayerHand.Entity
import Pitboss.Trace.Entity.PlayerSpot.Entity
import Pitboss.Trace.Entity.Table.Entity
import Pitboss.Trace.Entity.TableShoe.Entity
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.EntityId

type EOffering = Entity 'Offering
type ETable = Entity 'Table
type ETableShoe = Entity 'TableShoe
type EDealer = Entity 'Dealer
type EDealerRound = Entity 'DealerRound
type EDealerHand = Entity 'DealerHand
type EPlayer = Entity 'Player
type EPlayerSpot = Entity 'PlayerSpot
type EPlayerHand = Entity 'PlayerHand

mkOffering ::
    Meta (ClockedRef EOfferingId) ->
    EOfferingAttrs ->
    EOfferingModes ->
    EOfferingRels ->
    EOffering
mkOffering = EOffering

mkTable ::
    Meta (ClockedRef ETableId) ->
    ETableAttrs ->
    ETableModes ->
    ETableRels ->
    ETable
mkTable = ETable

mkTableShoe ::
    Meta (ClockedRef ETableShoeId) ->
    ETableShoeAttrs ->
    ETableShoeModes ->
    ETableShoeRels ->
    ETableShoe
mkTableShoe = ETableShoe

mkDealer ::
    Meta (ClockedRef EDealerId) ->
    EDealerAttrs ->
    EDealerModes ->
    EDealerRels ->
    EDealer
mkDealer = EDealer

mkDealerRound ::
    Meta (ClockedRef EDealerRoundId) ->
    EDealerRoundAttrs ->
    EDealerRoundModes ->
    EDealerRoundRels ->
    EDealerRound
mkDealerRound = EDealerRound

mkDealerHand ::
    Meta (ClockedRef EDealerHandId) ->
    EDealerHandAttrs ->
    EDealerHandModes ->
    EDealerHandRels ->
    EDealerHand
mkDealerHand = EDealerHand

mkPlayer ::
    Meta (ClockedRef EPlayerId) ->
    EPlayerAttrs ->
    EPlayerModes ->
    EPlayerRels ->
    EPlayer
mkPlayer = EPlayer

mkPlayerSpot ::
    Meta (ClockedRef EPlayerSpotId) ->
    EPlayerSpotAttrs ->
    EPlayerSpotModes ->
    EPlayerSpotRels ->
    EPlayerSpot
mkPlayerSpot = EPlayerSpot

mkPlayerHand ::
    Meta (ClockedRef EPlayerHandId) ->
    EPlayerHandAttrs ->
    EPlayerHandModes ->
    EPlayerHandRels ->
    EPlayerHand
mkPlayerHand = EPlayerHand

data Entity k where
    EDealer ::
        Meta (ClockedRef EDealerId) ->
        EDealerAttrs ->
        EDealerModes ->
        EDealerRels ->
        Entity 'Dealer
    EDealerRound ::
        Meta (ClockedRef EDealerRoundId) ->
        EDealerRoundAttrs ->
        EDealerRoundModes ->
        EDealerRoundRels ->
        Entity 'DealerRound
    EDealerHand ::
        Meta (ClockedRef EDealerHandId) ->
        EDealerHandAttrs ->
        EDealerHandModes ->
        EDealerHandRels ->
        Entity 'DealerHand
    EOffering ::
        Meta (ClockedRef EOfferingId) ->
        EOfferingAttrs ->
        EOfferingModes ->
        EOfferingRels ->
        Entity 'Offering
    EPlayer ::
        Meta (ClockedRef EPlayerId) ->
        EPlayerAttrs ->
        EPlayerModes ->
        EPlayerRels ->
        Entity 'Player
    EPlayerSpot ::
        Meta (ClockedRef EPlayerSpotId) ->
        EPlayerSpotAttrs ->
        EPlayerSpotModes ->
        EPlayerSpotRels ->
        Entity 'PlayerSpot
    EPlayerHand ::
        Meta (ClockedRef EPlayerHandId) ->
        EPlayerHandAttrs ->
        EPlayerHandModes ->
        EPlayerHandRels ->
        Entity 'PlayerHand
    ETable ::
        Meta (ClockedRef ETableId) ->
        ETableAttrs ->
        ETableModes ->
        ETableRels ->
        Entity 'Table
    ETableShoe ::
        Meta (ClockedRef ETableShoeId) ->
        ETableShoeAttrs ->
        ETableShoeModes ->
        ETableShoeRels ->
        Entity 'TableShoe

deriving instance Show (Entity k)
deriving instance Eq (Entity k)

data ESome where
    ESome :: Entity k -> ESome

instance ToJSON ESome where
    toJSON (ESome e) = case e of
        EDealer m a mo r -> tagged "Dealer" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        EDealerHand m a mo r -> tagged "DealerHand" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        EDealerRound m a mo r -> tagged "DealerRound" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        EOffering m a mo r -> tagged "Offering" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        EPlayer m a mo r -> tagged "Player" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        EPlayerHand m a mo r -> tagged "PlayerHand" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        EPlayerSpot m a mo r -> tagged "PlayerSpot" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        ETable m a mo r -> tagged "Table" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
        ETableShoe m a mo r -> tagged "TableShoe" ["meta" .= m, "attrs" .= a, "modes" .= mo, "rels" .= r]
      where
        tagged :: Text -> [Pair] -> Value
        tagged tag fields = object ["tag" .= tag, "contents" .= object fields]

instance FromJSON ESome where
    parseJSON = withObject "ESome" $ \o -> do
        tag <- o .: "tag"
        case tag of
            "Dealer" -> ESome <$> (EDealer <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "DealerHand" -> ESome <$> (EDealerHand <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "DealerRound" -> ESome <$> (EDealerRound <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "Offering" -> ESome <$> (EOffering <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "Player" -> ESome <$> (EPlayer <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "PlayerHand" -> ESome <$> (EPlayerHand <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "PlayerSpot" -> ESome <$> (EPlayerSpot <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "Table" -> ESome <$> (ETable <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            "TableShoe" -> ESome <$> (ETableShoe <$> o .: "meta" <*> o .: "attrs" <*> o .: "modes" <*> o .: "rels")
            other -> fail $ "Unknown tag in ESome: " ++ other

entityKind :: Entity k -> EntityKind
entityKind = \case
    EDealer{} -> Dealer
    EDealerHand{} -> DealerHand
    EDealerRound{} -> DealerRound
    EOffering{} -> Offering
    EPlayer{} -> Player
    EPlayerHand{} -> PlayerHand
    EPlayerSpot{} -> PlayerSpot
    ETable{} -> Table
    ETableShoe{} -> TableShoe
