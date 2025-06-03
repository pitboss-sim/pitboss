{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Simulation.Agency.Intent.Types where

import Pitboss.Blackjack
import Pitboss.Causality
import Pitboss.FSM

data family IntentCtx (k :: IntentKind)

data instance IntentCtx 'IPlayerHit = PlayerHitCtx
    { phICtxPlayerHand :: SomeHand
    , phICtxPlayerHandFSM :: SomePlayerHandFSM
    , phICtxIsPlayersTurn :: Bool
    , phICtxShoeHasCards :: Bool
    }
    deriving (Eq, Show)

data instance IntentCtx 'IPlayerStand = PlayerStandCtx
    { psICtxPlayerHand :: SomeHand
    , psICtxPlayerHandFSM :: SomePlayerHandFSM
    , psICtxIsPlayersTurn :: Bool
    }
    deriving (Eq, Show)

data instance IntentCtx 'IPlayerDouble = PlayerDoubleCtx
    { pdICtxPlayerHand :: SomeHand
    , pdICtxPlayerHandFSM :: SomePlayerHandFSM
    , pdICtxIsPlayersTurn :: Bool
    , pdICtxOffering :: Offering
    , pdICtxPlayerBankroll :: Chips
    , pdICtxCurrentBet :: Chips
    }
    deriving (Eq, Show)

data instance IntentCtx 'IPlayerSplit = PlayerSplitCtx
    { pspICtxPlayerHand :: SomeHand
    , pspICtxPlayerHandFSM :: SomePlayerHandFSM
    , pspICtxIsPlayersTurn :: Bool
    , pspICtxOffering :: Offering
    , pspICtxPlayerBankroll :: Chips
    , pspICtxCurrentBet :: Chips
    , pspICtxCurrentSplitCount :: Int
    }
    deriving (Eq, Show)

data instance IntentCtx 'IPlayerSurrender = PlayerSurrenderCtx
    { psrICtxPlayerHand :: SomeHand
    , psrICtxPlayerHandFSM :: SomePlayerHandFSM
    , psrICtxIsPlayersTurn :: Bool
    , psrICtxOffering :: Offering
    , psrICtxRoundPhase :: Maybe DealerRoundFSM
    }
    deriving (Eq, Show)

data instance IntentCtx 'IDealerHit = DealerHitCtx
    { dhICtxDealerHand :: SomeHand
    , dhICtxDealerHandFSM :: SomeDealerHandFSM
    , dhICtxGameRules :: GameRuleSet
    }
    deriving (Eq, Show)

data instance IntentCtx 'IDealerStand = DealerStandCtx
    { dsICtxDealerHand :: SomeHand
    , dsICtxDealerHandFSM :: SomeDealerHandFSM
    , dsICtxGameRules :: GameRuleSet
    }
    deriving (Eq, Show)

data instance IntentCtx 'IDealerDeal = DealerDealCtx
    { ddICtxTargetHand :: Either (EntityId 'PlayerHand) (EntityId 'DealerHand)
    , ddICtxTargetHandFSM :: Either SomePlayerHandFSM SomeDealerHandFSM
    , ddICtxShoeCardsRemaining :: Int
    , ddICtxRoundPhase :: Maybe DealerRoundFSM
    , ddICtxNextCard :: Card
    }
    deriving (Eq, Show)

data instance IntentCtx 'IDealerSettleBout = DealerSettleBoutCtx
    { dsbICtxBoutId :: EntityId 'Bout
    , dsbICtxPlayerHand :: SomeHand
    , dsbICtxDealerHand :: SomeHand
    , dsbICtxWager :: Chips
    , dsbICtxOffering :: Offering
    }
    deriving (Eq, Show)

data instance IntentCtx 'IDealerSettleInsurance = DealerSettleInsuranceCtx
    { dsiICtxPlayerIds :: [EntityId 'Player]
    , dsiICtxInsuranceBets :: [(EntityId 'Player, Chips)]
    , dsiICtxDealerHasBlackjack :: Bool
    }
    deriving (Eq, Show)
