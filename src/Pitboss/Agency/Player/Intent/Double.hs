{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Agency.Player.Intent.Double where

import Control.Lens ((^.))
import Control.Monad.Reader
import Pitboss.Blackjack.Materia.Chips (Chips(..))
import Pitboss.Blackjack.Materia.Hand (SomeHand)
import Pitboss.Blackjack.Offering (Offering)
import Pitboss.Blackjack.Play (canDoubleSomeHand)
import Pitboss.FSM.PlayerHand (SomePlayerHandFSM(..), PlayerHandFSM (..))
import Pitboss.State.Entity.Lenses
import Pitboss.State.Entity.Types
import Pitboss.State.TickCache (TickCacheContext, deref)
import Pitboss.State.Types.Core
import Prelude hiding (round)

-- Context for validating PlayerDoubleIntent
data PlayerDoubleContext = PlayerDoubleContext
    { pdcPlayerHand :: SomeHand
    , pdcPlayerHandFSM :: SomePlayerHandFSM
    , pdcIsPlayersTurn :: Bool
    , pdcOffering :: Offering
    , pdcPlayerBankroll :: Chips
    , pdcCurrentBet :: Chips
    }
    deriving (Eq, Show)

-- Build context from intent
buildContext ::
    EntityId 'Intent ->
    Reader TickCacheContext (Maybe PlayerDoubleContext)
buildContext intentId = do
    intent <- deref intentId
    case intent of
        Just (EIntent _ _ rels) -> do
            -- Get bout from intent
            bout <- maybe (pure Nothing) deref (_intentRelsTargetBout rels)

            -- Get player hand from bout
            playerHand <- case bout of
                Just b -> deref (b ^. bRels . bRelsPlayerHand)
                Nothing -> pure Nothing

            -- Get player (via hand -> spot -> player)
            player <- case playerHand of
                Just ph -> do
                    spot <- deref (ph ^. phRels . phRelsBelongsToPlayerSpot)
                    case spot of
                        Just s -> deref (s ^. psRels . psEntityRelsPlayerId)
                        Nothing -> pure Nothing
                Nothing -> pure Nothing

            -- Get table for offering (via bout -> round -> shoe -> table)
            table <- case bout of
                Just b -> do
                    round <- deref (b ^. bRels . bRelsDealerRound)
                    shoe <- case round of
                        Just r -> deref (r ^. drRels . drRelsTableShoeUsed)
                        Nothing -> pure Nothing
                    case shoe of
                        Just s -> deref (s ^. tsRels . tsRelsTable)
                        Nothing -> pure Nothing
                Nothing -> pure Nothing

            case (playerHand, player, table) of
                (Just ph, Just p, Just t) ->
                    pure $ Just PlayerDoubleContext
                        { pdcPlayerHand = ph ^. phAttrs . phAttrsHand
                        , pdcPlayerHandFSM = ph ^. phModes . phFsm
                        , pdcIsPlayersTurn = True -- TODO: derive from bout/round state
                        , pdcOffering = t ^. tAttrs . tAttrsOffering
                        , pdcPlayerBankroll = p ^. pAttrs . pAttrsBankroll
                        , pdcCurrentBet = ph ^. phAttrs . phAttrsOriginalBet
                        }
                _ -> pure Nothing
        Nothing -> pure Nothing

-- Validate the intent
validate :: PlayerDoubleContext -> Either String ()
validate ctx = do
    checkIsPlayersTurn ctx
    checkCanDouble ctx
    checkHasFundsToDouble ctx
    checkCanMakeDecision ctx

checkIsPlayersTurn :: PlayerDoubleContext -> Either String ()
checkIsPlayersTurn ctx =
    if pdcIsPlayersTurn ctx
        then Right ()
        else Left "Not player's turn"

checkCanDouble :: PlayerDoubleContext -> Either String ()
checkCanDouble ctx =
    if canDoubleSomeHand (pdcPlayerHand ctx) (pdcOffering ctx)
        then Right ()
        else Left "Cannot double this hand per table rules"

checkHasFundsToDouble :: PlayerDoubleContext -> Either String ()
checkHasFundsToDouble ctx =
    let (Chips bankroll) = pdcPlayerBankroll ctx
        (Chips bet) = pdcCurrentBet ctx
    in if bankroll >= bet
        then Right ()
        else Left "Insufficient funds to double"

checkCanMakeDecision :: PlayerDoubleContext -> Either String ()
checkCanMakeDecision ctx = case pdcPlayerHandFSM ctx of
    SomePlayerHandFSM DecisionFSM -> Right ()
    _ -> Left "Hand not in initial decision state"
