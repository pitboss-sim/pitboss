{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.State.Entity.Types.Id (
    EntityRef (..),
    Id,
    module Pitboss.State.Entity.Types.Id.Uid,
) where

import Control.Monad (when)
import Data.Aeson (FromJSON (..), KeyValue (..), ToJSON (..), object, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Data (Proxy (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Pitboss.State.Entity.Types
import Pitboss.State.Entity.Types.Id.Uid

type family Id (k :: EntityKind)
type instance Id k = Uid

class KnownEntityKind (k :: EntityKind) where
    entityTag :: Proxy k -> String

newtype TaggedId (k :: EntityKind) = TaggedId Uid

instance KnownEntityKind 'Dealer where
    entityTag _ = "Dealer"
instance KnownEntityKind 'DealerHand where
    entityTag _ = "DealerHand"
instance KnownEntityKind 'DealerRound where
    entityTag _ = "DealerRound"
instance KnownEntityKind 'Offering where
    entityTag _ = "Offering"
instance KnownEntityKind 'Player where
    entityTag _ = "Player"
instance KnownEntityKind 'PlayerHand where
    entityTag _ = "PlayerHand"
instance KnownEntityKind 'PlayerSpot where
    entityTag _ = "PlayerSpot"
instance KnownEntityKind 'Table where
    entityTag _ = "Table"
instance KnownEntityKind 'TableShoe where
    entityTag _ = "TableShoe"

instance (KnownEntityKind k) => ToJSON (TaggedId k) where
    toJSON (TaggedId uid) =
        object
            [ "tag" .= entityTag (Proxy @k)
            , "uid" .= uid
            ]

instance (KnownEntityKind k) => FromJSON (TaggedId k) where
    parseJSON = withObject "TaggedId" $ \o -> do
        tag <- o .: "tag" :: Parser T.Text
        uid <- o .: "uid"
        let expected = T.pack $ entityTag (Proxy @k)
        when (tag /= expected) $
            fail $
                "Expected tag " ++ T.unpack expected ++ ", but got " ++ T.unpack tag
        pure $ TaggedId uid

data Clocked a = Clocked' Tick a
    deriving (Eq, Ord, Show, Generic)

newtype EntityRef (k :: EntityKind)
    = Clocked (Id k)
    deriving (Eq, Ord, Show, Generic)

instance (ToJSON (EntityRef k)) => ToJSON (EntityRef k) where
    toJSON (Clocked x) = object ["variant" .= ("Clocked" :: String), "value" .= x]

instance (FromJSON (EntityRef k)) => FromJSON (EntityRef k) where
    parseJSON = withObject "EntityRef" $ \o -> do
        variant <- o .: "variant"
        case variant :: String of
            "Clocked" -> Clocked <$> o .: "value"
            _ -> fail ("Unknown EntityRef variant: " ++ variant)
