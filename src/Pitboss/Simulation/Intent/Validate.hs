{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Intent.Validate where

import Pitboss.Causality.Types.Core
import Pitboss.Simulation.Intent.Types

data ValidationResult = Valid | Invalid String
    deriving (Eq, Show)

validateBettingPlayerIntent :: PlayerIntent -> IntentCtx 'BettingPlayer -> ValidationResult
validateBettingPlayerIntent intent ctx = case intent of
    PlaceBet amount ->
        let _ = bpcSpine ctx
            (minBet, maxBet) = bpcTableLimits ctx
            availableFunds = bpcAvailableFunds ctx
         in if amount < minBet
                then Invalid "Bet below table minimum"
                else
                    if amount > maxBet
                        then Invalid "Bet above table maximum"
                        else
                            if amount > availableFunds
                                then Invalid "Insufficient funds"
                                else Valid
    _ -> Invalid "Invalid intent for betting player"

validatePlayingPlayerIntent :: PlayerIntent -> IntentCtx 'PlayingPlayer -> ValidationResult
validatePlayingPlayerIntent intent ctx = case intent of
    Hit' ->
        if Hit `elem` ppcAvailableMoves ctx
            then Valid
            else Invalid "Hit not available"
    Stand' ->
        if Stand `elem` ppcAvailableMoves ctx
            then Valid
            else Invalid "Stand not available"
    Double' ->
        if Double `elem` ppcAvailableMoves ctx
            then Valid
            else Invalid "Double not available"
    Split' ->
        if Split `elem` ppcAvailableMoves ctx
            then Valid
            else Invalid "Split not available"
    Surrender' ->
        if Surrender `elem` ppcAvailableMoves ctx
            then Valid
            else Invalid "Surrender not available"
    _ -> Invalid "Invalid intent for playing player"

validatePlayingDealerIntent :: DealerIntent -> IntentCtx 'PlayingDealer -> ValidationResult
validatePlayingDealerIntent intent ctx = case intent of
    DealerHit ->
        if Hit `elem` pdcAvailableMoves ctx
            then Valid
            else Invalid "Dealer hit not available"
    DealerStand ->
        if Stand `elem` pdcAvailableMoves ctx
            then Valid
            else Invalid "Dealer stand not available"
    _ -> Invalid "Invalid intent for playing dealer"
