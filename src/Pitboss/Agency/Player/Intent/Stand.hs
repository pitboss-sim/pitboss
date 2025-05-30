{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Agency.Player.Intent.Stand where

import Control.Lens ((^.))
import Control.Monad.Reader
import Pitboss.Blackjack.Play (isBusted)
import Pitboss.Blackjack.Materia.Hand (SomeHand)
import Pitboss.FSM.PlayerHand (SomePlayerHandFSM(..), PlayerHandFSM (..))
import Pitboss.State.Entity.Lenses
import Pitboss.State.Entity.Types
import Pitboss.State.TickCache (TickCacheContext, deref)
import Pitboss.State.Types.Core
import Prelude hiding (round)

-- Context for validating PlayerStandIntent
data PlayerStandContext = PlayerStandContext
    { pscPlayerHand :: SomeHand
    , pscPlayerHandFSM :: SomePlayerHandFSM
    , pscIsPlayersTurn :: Bool
    }
    deriving (Eq, Show)

-- Build context from intent
buildContext ::
    EntityId 'Intent ->
    Reader TickCacheContext (Maybe PlayerStandContext)
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

            case playerHand of
                Just ph -> pure $ Just PlayerStandContext
                    { pscPlayerHand = ph ^. phAttrs . phAttrsHand
                    , pscPlayerHandFSM = ph ^. phModes . phFsm
                    , pscIsPlayersTurn = True -- TODO: derive from bout/round state
                    }
                Nothing -> pure Nothing
        Nothing -> pure Nothing

-- Validate the intent
validate :: PlayerStandContext -> Either String ()
validate ctx = do
    checkIsPlayersTurn ctx
    checkHandNotBusted ctx
    checkCanMakeDecision ctx

checkIsPlayersTurn :: PlayerStandContext -> Either String ()
checkIsPlayersTurn ctx =
    if pscIsPlayersTurn ctx
        then Right ()
        else Left "Not player's turn"

checkHandNotBusted :: PlayerStandContext -> Either String ()
checkHandNotBusted ctx =
    if not (isBusted (pscPlayerHand ctx))
        then Right ()
        else Left "Cannot stand on busted hand"

checkCanMakeDecision :: PlayerStandContext -> Either String ()
checkCanMakeDecision ctx = case pscPlayerHandFSM ctx of
    SomePlayerHandFSM DecisionFSM -> Right ()
    SomePlayerHandFSM HittingFSM -> Right ()
    _ -> Left "Hand not in a state where standing is allowed"
