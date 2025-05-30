{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Agency.Archetype.Dealer where

import Data.Aeson (FromJSON, ToJSON, object, toJSON, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, parseJSON)
import GHC.Generics (Generic)

data DealerArchetype where
    ByTheBookDealer :: PenetrationProfile -> DealerArchetype
    RookieDealer :: ErrorProfile -> PenetrationProfile -> DealerArchetype
    VeteranDealer :: PaceProfile -> PenetrationProfile -> DealerArchetype

data PenetrationProfile = PenetrationProfile
    { targetPenetration :: Double -- TODO: no floating point
    , penetrationVariance :: Double -- TODO: no floating point
    }
    deriving (Eq, Show, Generic)

data ErrorProfile = ErrorProfile
    { errorRate :: Double -- TODO: no floating point
    , errorDistribution :: ErrorDistribution
    }
    deriving (Eq, Show, Generic)

data ErrorDistribution = ErrorDistribution
    { misreadHand :: Double -- TODO: no floating point
    , incorrectPayout :: Double -- TODO: no floating point
    , exposedCard :: Double -- TODO: no floating point
    }
    deriving (Eq, Show, Generic)

data PaceProfile = PaceProfile
    { handsPerHour :: Int
    , paceVariance :: Double -- TODO: no floating point
    }
    deriving (Eq, Show, Generic)

data SomeDealerArchetype where
    SomeDealerArchetype :: DealerArchetype -> SomeDealerArchetype

-- Helper constructors
perfectDealer :: DealerArchetype
perfectDealer = ByTheBookDealer standardPenetration

standardPenetration :: PenetrationProfile
standardPenetration = PenetrationProfile 0.75 0.05

noErrors :: ErrorProfile
noErrors = ErrorProfile 0.0 (ErrorDistribution 0.0 0.0 0.0)

rookieErrors :: ErrorProfile
rookieErrors = ErrorProfile 0.02 (ErrorDistribution 0.5 0.3 0.2)

steadyPace :: PaceProfile
steadyPace = PaceProfile 80 0.1

fastPace :: PaceProfile
fastPace = PaceProfile 100 0.15

-- Instances
instance Eq SomeDealerArchetype where
    SomeDealerArchetype (ByTheBookDealer p1) == SomeDealerArchetype (ByTheBookDealer p2) = p1 == p2
    SomeDealerArchetype (RookieDealer e1 p1) == SomeDealerArchetype (RookieDealer e2 p2) = e1 == e2 && p1 == p2
    SomeDealerArchetype (VeteranDealer pa1 pe1) == SomeDealerArchetype (VeteranDealer pa2 pe2) = pa1 == pa2 && pe1 == pe2
    _ == _ = False

instance Show SomeDealerArchetype where
    show (SomeDealerArchetype (ByTheBookDealer pen)) =
        "ByTheBookDealer (penetration: " ++ show (targetPenetration pen) ++ ")"
    show (SomeDealerArchetype (RookieDealer err pen)) =
        "RookieDealer (error rate: " ++ show (errorRate err) ++ ", penetration: " ++ show (targetPenetration pen) ++ ")"
    show (SomeDealerArchetype (VeteranDealer pace pen)) =
        "VeteranDealer (HPH: " ++ show (handsPerHour pace) ++ ", penetration: " ++ show (targetPenetration pen) ++ ")"

instance ToJSON PenetrationProfile
instance FromJSON PenetrationProfile
instance ToJSON ErrorProfile
instance FromJSON ErrorProfile
instance ToJSON ErrorDistribution
instance FromJSON ErrorDistribution
instance ToJSON PaceProfile
instance FromJSON PaceProfile

instance ToJSON SomeDealerArchetype where
    toJSON (SomeDealerArchetype (ByTheBookDealer pen)) =
        object
            [ "type" .= ("ByTheBook" :: String)
            , "penetration" .= pen
            ]
    toJSON (SomeDealerArchetype (RookieDealer err pen)) =
        object
            [ "type" .= ("Rookie" :: String)
            , "errors" .= err
            , "penetration" .= pen
            ]
    toJSON (SomeDealerArchetype (VeteranDealer pace pen)) =
        object
            [ "type" .= ("Veteran" :: String)
            , "pace" .= pace
            , "penetration" .= pen
            ]

instance FromJSON SomeDealerArchetype where
    parseJSON = withObject "SomeDealerArchetype" $ \o -> do
        typ <- o .: "type" :: Parser String
        case typ of
            "ByTheBook" -> do
                pen <- o .: "penetration"
                pure $ SomeDealerArchetype (ByTheBookDealer pen)
            "Rookie" -> do
                err <- o .: "errors"
                pen <- o .: "penetration"
                pure $ SomeDealerArchetype (RookieDealer err pen)
            "Veteran" -> do
                pace <- o .: "pace"
                pen <- o .: "penetration"
                pure $ SomeDealerArchetype (VeteranDealer pace pen)
            _ -> fail $ "Unknown dealer archetype: " ++ typ
