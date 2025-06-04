{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Simulation.Intent.Types where

import Pitboss.Causality

data family IntentCtx (k :: IntentKind)

data instance IntentCtx 'IPlayerHit = PlayerHitCtx
    { phICtxContestantId :: ContestantId
    , phICtxHandId :: HandId
    }
    deriving (Eq, Show)

data instance IntentCtx 'IPlayerStand = PlayerStandCtx
    { psICtxContestantId :: ContestantId
    , psICtxHandId :: HandId
    }
    deriving (Eq, Show)

data instance IntentCtx 'IPlayerDouble = PlayerDoubleCtx
    { pdICtxContestantId :: ContestantId
    , pdICtxHandId :: HandId
    , pdICtxPlayerId :: PlayerId
    }
    deriving (Eq, Show)

data instance IntentCtx 'IPlayerSplit = PlayerSplitCtx
    { pspICtxContestantId :: ContestantId
    , pspICtxHandId :: HandId
    , pspICtxPlayerId :: PlayerId
    }
    deriving (Eq, Show)

data instance IntentCtx 'IPlayerSurrender = PlayerSurrenderCtx
    { psrICtxContestantId :: ContestantId
    , psrICtxHandId :: HandId
    }
    deriving (Eq, Show)

data instance IntentCtx 'IDealerHit = DealerHitCtx
    { dhICtxDealerId :: DealerId
    , dhICtxHandId :: HandId
    }
    deriving (Eq, Show)

data instance IntentCtx 'IDealerStand = DealerStandCtx
    { dsICtxDealerId :: DealerId
    , dsICtxHandId :: HandId
    }
    deriving (Eq, Show)

data instance IntentCtx 'IDealerDeal = DealerDealCtx
    { ddICtxDealerId :: DealerId
    , ddICtxHandId :: HandId
    , ddICtxShoeId :: ShoeId
    }
    deriving (Eq, Show)

data instance IntentCtx 'IDealerSettleBout = DealerSettleBoutCtx
    { dsbICtxDealerId :: DealerId
    , dsbICtxBoutId :: BoutId
    }
    deriving (Eq, Show)

data instance IntentCtx 'IDealerSettleInsurance = DealerSettleInsuranceCtx
    { dsiICtxDealerId :: DealerId
    , dsiICtxPlayerIds :: [PlayerId]
    }
    deriving (Eq, Show)
