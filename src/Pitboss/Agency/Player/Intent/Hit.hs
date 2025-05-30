{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Agency.Player.Intent.Hit where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Map.Strict qualified as Map
import Pitboss.Blackjack.Materia.Hand (SomeHand, handScore)
import Pitboss.Blackjack.Play (isBusted)
import Pitboss.FSM.PlayerHand (SomePlayerHandFSM(..), PlayerHandFSM (..))
import Pitboss.State.Entity.Lenses
import Pitboss.State.Entity.Types
import Pitboss.State.TickCache (TickCacheContext, deref)
import Pitboss.State.Types.Core
import Data.Map.Strict
import Prelude hiding (round)

-- Context for validating PlayerHitIntent
data PlayerHitContext = PlayerHitContext
    { phcPlayerHand :: SomeHand
    , phcPlayerHandFSM :: SomePlayerHandFSM
    , phcIsPlayersTurn :: Bool
    , phcShoeHasCards :: Bool
    }
    deriving (Eq, Show)

-- Build context from intent
buildContext ::
    EntityId 'Intent ->
    Reader TickCacheContext (Maybe PlayerHitContext)
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

            -- Check shoe availability (via bout -> round -> shoe)
            shoeHasCards <- case bout of
                Just b -> do
                    round <- deref (b ^. bRels . bRelsDealerRound)
                    shoe <- case round of
                        Just r -> deref (r ^. drRels . drRelsTableShoeUsed)
                        Nothing -> pure Nothing
                    pure $ case shoe of
                        Just s -> hasAvailableCards (s ^. tsAttrs . tsAttrsCardStates)
                        Nothing -> False
                Nothing -> pure False

            case playerHand of
                Just ph -> pure $ Just PlayerHitContext
                    { phcPlayerHand = ph ^. phAttrs . phAttrsHand
                    , phcPlayerHandFSM = ph ^. phModes . phFsm
                    , phcIsPlayersTurn = True -- TODO: derive from bout/round state
                    , phcShoeHasCards = shoeHasCards
                    }
                Nothing -> pure Nothing
        Nothing -> pure Nothing

-- Validate the intent
validate :: PlayerHitContext -> Either String ()
validate ctx = do
    checkIsPlayersTurn ctx
    checkHandNotBusted ctx
    checkCanMakeDecision ctx
    checkShoeHasCards ctx
    checkNotAt21 ctx

checkIsPlayersTurn :: PlayerHitContext -> Either String ()
checkIsPlayersTurn ctx =
    if phcIsPlayersTurn ctx
        then Right ()
        else Left "Not player's turn"

checkHandNotBusted :: PlayerHitContext -> Either String ()
checkHandNotBusted ctx =
    if not (isBusted (phcPlayerHand ctx))
        then Right ()
        else Left "Cannot hit on busted hand"

checkCanMakeDecision :: PlayerHitContext -> Either String ()
checkCanMakeDecision ctx = case phcPlayerHandFSM ctx of
    SomePlayerHandFSM DecisionFSM -> Right ()
    SomePlayerHandFSM HittingFSM -> Right ()
    _ -> Left "Hand not in a state where hitting is allowed"

checkShoeHasCards :: PlayerHitContext -> Either String ()
checkShoeHasCards ctx =
    if phcShoeHasCards ctx
        then Right ()
        else Left "No cards available in shoe"

checkNotAt21 :: PlayerHitContext -> Either String ()
checkNotAt21 ctx =
    if handScore (phcPlayerHand ctx) < 21
        then Right ()
        else Left "Cannot hit when hand totals 21"

-- Helper
hasAvailableCards :: Map CardIx CardState -> Bool
hasAvailableCards cardStates =
    any notDealt (Map.elems cardStates)
  where
    notDealt InHand = False
    notDealt InDiscard = False
    notDealt Burned = False
