{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Simulation.Intent.Context.Types.Core where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Types.Core

data family Spine (r :: ActorRole) :: Type

data instance Spine 'BettingPlayer = BettingPlayerSpine
    { bpsTable :: EntityState 'Table
    , bpsPlayer :: EntityState 'Player
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance Spine 'PlayingPlayer = PlayingPlayerSpine
    { ppsTable :: EntityState 'Table
    , ppsRound :: EntityState 'Round
    , ppsBout :: EntityState 'Bout
    , ppsPlayer :: EntityState 'Player
    , ppsDealer :: EntityState 'Dealer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance Spine 'ObservingPlayer = ObservingPlayerSpine
    { opsTable :: EntityState 'Table
    , opsRound :: EntityState 'Round
    , opsBout :: EntityState 'Bout
    , opsPlayer :: EntityState 'Player
    , opsDealer :: EntityState 'Dealer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance Spine 'SeatedPlayer = SeatedPlayerSpine
    { spsTable :: EntityState 'Table
    , spsPlayer :: EntityState 'Player
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance Spine 'BetweenRoundsDealer = BetweenRoundsDealerSpine
    { brdsTable :: EntityState 'Table
    , brdsDealer :: EntityState 'Dealer
    , brdsShoe :: EntityState 'Shoe
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance Spine 'DealingDealer = DealingDealerSpine
    { ddsTable :: EntityState 'Table
    , ddsRound :: EntityState 'Round
    , ddsBout :: EntityState 'Bout
    , ddsPlayer :: EntityState 'Player
    , ddsDealer :: EntityState 'Dealer
    , ddsShoe :: EntityState 'Shoe
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance Spine 'PlayingDealer = PlayingDealerSpine
    { pdsTable :: EntityState 'Table
    , pdsRound :: EntityState 'Round
    , pdsBout :: EntityState 'Bout
    , pdsPlayer :: EntityState 'Player
    , pdsDealer :: EntityState 'Dealer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance Spine 'SettlingDealer = SettlingDealerSpine
    { sdsTable :: EntityState 'Table
    , sdsRound :: EntityState 'Round
    , sdsBout :: EntityState 'Bout
    , sdsPlayer :: EntityState 'Player
    , sdsDealer :: EntityState 'Dealer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
