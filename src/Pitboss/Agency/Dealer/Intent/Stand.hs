{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Agency.Dealer.Intent.Stand where

import Control.Lens ((^.))
import Control.Monad.Reader
import Pitboss.Blackjack.Materia.Hand (SomeHand)
import Pitboss.Blackjack.Offering (gameRuleSet)
import Pitboss.Blackjack.Offering.RuleSet (GameRuleSet)
import Pitboss.FSM.DealerHand (SomeDealerHandFSM(..), DealerHandFSM (..))
import Pitboss.FSM.DealerHand.Transition (dealerShouldHit)
import Pitboss.State.Entity.Lenses
import Pitboss.State.Entity.Types
import Pitboss.State.TickCache (TickCacheContext, deref)
import Pitboss.State.Types.Core
import Prelude hiding (round)

-- Context for validating DealerStandIntent
data DealerStandContext = DealerStandContext
    { dscDealerHand :: SomeHand
    , dscDealerHandFSM :: SomeDealerHandFSM
    , dscGameRules :: GameRuleSet
    }
    deriving (Eq, Show)

-- Build context from intent
buildContext ::
    EntityId 'Intent ->
    Reader TickCacheContext (Maybe DealerStandContext)
buildContext intentId = do
    intent <- deref intentId
    case intent of
        Just (EIntent _ _ rels) -> do
            -- Get bout from intent
            bout <- maybe (pure Nothing) deref (_intentRelsTargetBout rels)

            -- Get dealer hand from bout
            dealerHand <- case bout of
                Just b -> deref (b ^. bRels . bRelsDealerHand)
                Nothing -> pure Nothing

            -- Get table for rules (via bout -> round -> shoe -> table)
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

            case (dealerHand, table) of
                (Just dh, Just t) ->
                    let offering = t ^. tAttrs . tAttrsOffering
                    in pure $ Just DealerStandContext
                        { dscDealerHand = dh ^. dhAttrs . dhAttrsHand
                        , dscDealerHandFSM = dh ^. dhModes . dhModesDealerHand
                        , dscGameRules = gameRuleSet offering
                        }
                _ -> pure Nothing
        Nothing -> pure Nothing

-- Validate the intent
validate :: DealerStandContext -> Either String ()
validate ctx = do
    checkDealerMustStand ctx
    checkInDealingPhase ctx

checkDealerMustStand :: DealerStandContext -> Either String ()
checkDealerMustStand ctx =
    if not (dealerShouldHit (dscGameRules ctx) (dscDealerHand ctx))
        then Right ()
        else Left "Dealer must hit with this hand"

checkInDealingPhase :: DealerStandContext -> Either String ()
checkInDealingPhase ctx = case dscDealerHandFSM ctx of
    SomeDealerHandFSM DealingFSM -> Right ()
    _ -> Left "Dealer hand not in dealing phase"
