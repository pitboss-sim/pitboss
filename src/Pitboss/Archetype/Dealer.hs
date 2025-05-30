{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Archetype.Dealer where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data DealerArchetype
    = RoboticDealer
    | HumanDealer
        { _penetrationVariance :: PenetrationVariance
        , _proceduralErrors :: ErrorProfile
        , _paceVariance :: PaceProfile
        }
    deriving (Eq, Show, Generic)

data PenetrationVariance = PenetrationVariance
    { _estimationError :: Double
    , _consistency :: Double
    }
    deriving (Eq, Show, Generic)

data ErrorProfile = ErrorProfile
    { _errorRate :: Double
    , _errorTypes :: ErrorDistribution
    }
    deriving (Eq, Show, Generic)

data ErrorDistribution = ErrorDistribution
    { _misreadHand :: Double
    , _incorrectPayout :: Double
    , _prematureCollection :: Double
    , _exposedCard :: Double
    }
    deriving (Eq, Show, Generic)

data PaceProfile = PaceProfile
    { _baseHandsPerHour :: Int
    , _variability :: Double
    , _fatigueFactor :: Double
    }
    deriving (Eq, Show, Generic)

makeLenses ''PenetrationVariance
makeLenses ''ErrorProfile
makeLenses ''ErrorDistribution
makeLenses ''PaceProfile

instance ToJSON DealerArchetype
instance FromJSON DealerArchetype
instance ToJSON PenetrationVariance
instance FromJSON PenetrationVariance
instance ToJSON ErrorProfile
instance FromJSON ErrorProfile
instance ToJSON ErrorDistribution
instance FromJSON ErrorDistribution
instance ToJSON PaceProfile
instance FromJSON PaceProfile

data SomeDealerArchetype = SomeDealerArchetype DealerArchetype
    deriving (Eq, Show, Generic)

instance ToJSON SomeDealerArchetype
instance FromJSON SomeDealerArchetype

noErrors :: ErrorProfile
noErrors =
    ErrorProfile
        { _errorRate = 0.0
        , _errorTypes =
            ErrorDistribution
                { _misreadHand = 0.0
                , _incorrectPayout = 0.0
                , _prematureCollection = 0.0
                , _exposedCard = 0.0
                }
        }

perfectPenetration :: PenetrationVariance
perfectPenetration =
    PenetrationVariance
        { _estimationError = 0.0
        , _consistency = 1.0
        }

steadyPace :: PaceProfile
steadyPace =
    PaceProfile
        { _baseHandsPerHour = 60
        , _variability = 0.0
        , _fatigueFactor = 1.0
        }
