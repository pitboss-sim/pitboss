{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Pitboss.Simulation.Event.Generate where

import Pitboss.Simulation.Event
import Pitboss.Simulation.Intent.Types

intentToEvent :: SomeIntent -> SimulationEvent
intentToEvent intent = case intent of
    BoutPlayerHitIntent (BoutPlayerHitCtx{..}) -> Game $ BoutPlayerHit bphICtxBoutId
    BoutPlayerStandIntent (BoutPlayerStandCtx{..}) -> Game $ BoutPlayerStood bpsICtxBoutId
    BoutPlayerDoubleIntent (BoutPlayerDoubleCtx{..}) -> Game $ BoutPlayerDoubledDown bpdICtxBoutId
    BoutPlayerSplitIntent (BoutPlayerSplitCtx{..}) -> Game $ BoutPlayerSplit bpspICtxBoutId
    BoutPlayerSurrenderIntent (BoutPlayerSurrenderCtx{..}) -> Game $ BoutPlayerSurrendered bpsrICtxBoutId
    BoutDealerDrawIntent (BoutDealerDrawCtx{..}) -> Game $ CardDrawn bddrawICtxDealerId bddrawICtxShoeId (error "TODO: derive card from shoe state")
    BoutDealerDealIntent (BoutDealerDealCtx{..}) ->
        case bddICtxTarget of
            BoutPlayerTarget boutPlayerId -> Game $ BoutPlayerCardDealt bddICtxCard boutPlayerId
            BoutDealerTarget boutDealerId -> Game $ BoutDealerCardDealt bddICtxCard boutDealerId
    BoutDealerHitIntent (BoutDealerHitCtx{..}) -> Game $ BoutDealerHit bdhICtxBoutId
    BoutDealerStandIntent (BoutDealerStandCtx{..}) -> Game $ BoutDealerStood bdsICtxBoutId
    BoutDealerRevealIntent (BoutDealerRevealCtx{..}) -> Game $ BoutDealerRevealed bdrICtxBoutId
    BoutDealerSettleBoutIntent (BoutDealerSettleBoutCtx{..}) -> Game $ BoutSettled bdsbICtxBoutId (error "TODO: derive outcome from hand comparison")
    BoutDealerSettleInsuranceIntent (BoutDealerSettleInsuranceCtx{..}) -> Game $ InsuranceSettled bdsiICtxPlayerIds
