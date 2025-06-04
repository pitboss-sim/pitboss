{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Simulation.Intent.Types where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types.Core
import Pitboss.Causality.Types.Core
import Pitboss.Simulation.Intent.Context.Types.Core

data family IntentCtx (r :: ActorRole) :: Type

data instance IntentCtx 'BettingPlayer = BettingPlayerCtx
    { bpcSpine :: Spine 'BettingPlayer
    , bpcAvailableFunds :: Chips
    , bpcTableLimits :: (Chips, Chips)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance IntentCtx 'PlayingPlayer = PlayingPlayerCtx
    { ppcSpine :: Spine 'PlayingPlayer
    , ppcAvailableMoves :: [ValidMove]
    , ppcCurrentBet :: Chips
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance IntentCtx 'ObservingPlayer = ObservingPlayerCtx
    { opcSpine :: Spine 'ObservingPlayer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance IntentCtx 'SeatedPlayer = SeatedPlayerCtx
    { spcSpine :: Spine 'SeatedPlayer
    , spcAvailableFunds :: Chips
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance IntentCtx 'BetweenRoundsDealer = BetweenRoundsDealerCtx
    { brdcSpine :: Spine 'BetweenRoundsDealer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance IntentCtx 'DealingDealer = DealingDealerCtx
    { ddcSpine :: Spine 'DealingDealer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance IntentCtx 'PlayingDealer = PlayingDealerCtx
    { pdcSpine :: Spine 'PlayingDealer
    , pdcAvailableMoves :: [ValidMove]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance IntentCtx 'SettlingDealer = SettlingDealerCtx
    { sdcSpine :: Spine 'SettlingDealer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ValidMove
    = Hit
    | Stand
    | Double
    | Split
    | Surrender
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PlayerIntent
    = Hit'
    | Stand'
    | Double'
    | Split'
    | Surrender'
    | PlaceBet Chips
    | PlaceInsurance Chips
    | PlayHand HandIx
    | RequestBanking Chips
    | JoinTable TableId
    | TakeSeat TableSpotIx
    | BuyIn Chips
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DealerIntent
    = DealerHit
    | DealerStand
    | DealerReveal
    | StartRound
    | CollectCards
    | OfferInsurance
    | SettleBouts
    | PayoutWinnings
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ValidatedPlayerIntent (r :: ActorRole) where
    ValidatedPlayerIntent :: PlayerIntent -> IntentCtx r -> ValidatedPlayerIntent r

data ValidatedDealerIntent (r :: ActorRole) where
    ValidatedDealerIntent :: DealerIntent -> IntentCtx r -> ValidatedDealerIntent r

data SomeIntent where
    BoutPlayerHitIntent :: BoutPlayerHitCtx -> SomeIntent
    BoutPlayerStandIntent :: BoutPlayerStandCtx -> SomeIntent
    BoutPlayerDoubleIntent :: BoutPlayerDoubleCtx -> SomeIntent
    BoutPlayerSplitIntent :: BoutPlayerSplitCtx -> SomeIntent
    BoutPlayerSurrenderIntent :: BoutPlayerSurrenderCtx -> SomeIntent
    BoutDealerDrawIntent :: BoutDealerDrawCtx -> SomeIntent
    BoutDealerDealIntent :: BoutDealerDealCtx -> SomeIntent
    BoutDealerRevealIntent :: BoutDealerRevealCtx -> SomeIntent
    BoutDealerSettleInsuranceIntent :: BoutDealerSettleInsuranceCtx -> SomeIntent
    BoutDealerHitIntent :: BoutDealerHitCtx -> SomeIntent
    BoutDealerStandIntent :: BoutDealerStandCtx -> SomeIntent
    BoutDealerSettleBoutIntent :: BoutDealerSettleBoutCtx -> SomeIntent

data BoutPlayerHitCtx = BoutPlayerHitCtx {bphICtxBoutId :: BoutId} deriving (Eq, Show)
data BoutPlayerStandCtx = BoutPlayerStandCtx {bpsICtxBoutId :: BoutId} deriving (Eq, Show)
data BoutPlayerDoubleCtx = BoutPlayerDoubleCtx {bpdICtxBoutId :: BoutId, bpdICtxPlayerId :: PlayerId, bpdICtxTableId :: TableId} deriving (Eq, Show)
data BoutPlayerSplitCtx = BoutPlayerSplitCtx {bpspICtxBoutId :: BoutId, bpspICtxPlayerId :: PlayerId, bpspICtxTableId :: TableId} deriving (Eq, Show)
data BoutPlayerSurrenderCtx = BoutPlayerSurrenderCtx {bpsrICtxBoutId :: BoutId, bpsrICtxTableId :: TableId} deriving (Eq, Show)
data BoutDealerHitCtx = BoutDealerHitCtx {bdhICtxBoutId :: BoutId} deriving (Eq, Show)
data BoutDealerStandCtx = BoutDealerStandCtx {bdsICtxDealerId :: DealerId, bdsICtxBoutId :: BoutId} deriving (Eq, Show)
data BoutDealerDrawCtx = BoutDealerDrawCtx {bddrawICtxDealerId :: DealerId, bddrawICtxShoeId :: ShoeId, bddrawICtxTableId :: TableId} deriving (Eq, Show)
data BoutDealerDealCtx = BoutDealerDealCtx {bddICtxDealerId :: DealerId, bddICtxBoutId :: BoutId, bddICtxCard :: Card, bddICtxTarget :: CardDealTarget, bddICtxTableId :: TableId} deriving (Eq, Show)
data BoutDealerRevealCtx = BoutDealerRevealCtx {bdrICtxDealerId :: DealerId, bdrICtxBoutId :: BoutId} deriving (Eq, Show)
data BoutDealerSettleBoutCtx = BoutDealerSettleBoutCtx {bdsbICtxDealerId :: DealerId, bdsbICtxBoutId :: BoutId, bdsbICtxTableId :: TableId} deriving (Eq, Show)
data BoutDealerSettleInsuranceCtx = BoutDealerSettleInsuranceCtx {bdsiICtxDealerId :: DealerId, bdsiICtxPlayerIds :: [PlayerId], bdsiICtxTableId :: TableId} deriving (Eq, Show)

data CardDealTarget = BoutPlayerTarget BoutId | BoutDealerTarget BoutId deriving (Eq, Show)
