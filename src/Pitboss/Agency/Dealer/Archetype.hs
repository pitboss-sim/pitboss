{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Agency.Dealer.Archetype where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=), object, withObject, toJSON)
import Data.Aeson.Types (Parser, parseJSON)
import GHC.Generics (Generic)

-- Dealer archetypes
data DealerArchetype where
    ByTheBookDealer :: DealerArchetype
    RookieDealer :: ErrorRate -> DealerArchetype
    VeteranDealer :: PaceModifier -> DealerArchetype

newtype ErrorRate = ErrorRate Double
    deriving (Eq, Show, Generic)

newtype PaceModifier = PaceModifier Double
    deriving (Eq, Show, Generic)

data SomeDealerArchetype where
    SomeDealerArchetype :: DealerArchetype -> SomeDealerArchetype

instance Eq SomeDealerArchetype where
    SomeDealerArchetype ByTheBookDealer == SomeDealerArchetype ByTheBookDealer = True
    SomeDealerArchetype (RookieDealer r1) == SomeDealerArchetype (RookieDealer r2) = r1 == r2
    SomeDealerArchetype (VeteranDealer p1) == SomeDealerArchetype (VeteranDealer p2) = p1 == p2
    _ == _ = False

instance Show SomeDealerArchetype where
    show (SomeDealerArchetype ByTheBookDealer) = "ByTheBookDealer"
    show (SomeDealerArchetype (RookieDealer rate)) = "RookieDealer (" ++ show rate ++ ")"
    show (SomeDealerArchetype (VeteranDealer pace)) = "VeteranDealer (" ++ show pace ++ ")"

instance ToJSON ErrorRate
instance FromJSON ErrorRate

instance ToJSON PaceModifier
instance FromJSON PaceModifier

instance ToJSON SomeDealerArchetype where
    toJSON (SomeDealerArchetype ByTheBookDealer) =
        object ["type" .= ("ByTheBook" :: String)]
    toJSON (SomeDealerArchetype (RookieDealer rate)) =
        object ["type" .= ("Rookie" :: String), "errorRate" .= rate]
    toJSON (SomeDealerArchetype (VeteranDealer pace)) =
        object ["type" .= ("Veteran" :: String), "paceModifier" .= pace]

instance FromJSON SomeDealerArchetype where
    parseJSON = withObject "SomeDealerArchetype" $ \o -> do
        typ <- o .: "type" :: Parser String
        case typ of
            "ByTheBook" -> pure $ SomeDealerArchetype ByTheBookDealer
            "Rookie" -> do
                rate <- o .: "errorRate"
                pure $ SomeDealerArchetype (RookieDealer rate)
            "Veteran" -> do
                pace <- o .: "paceModifier"
                pure $ SomeDealerArchetype (VeteranDealer pace)
            _ -> fail $ "Unknown dealer archetype: " ++ typ
