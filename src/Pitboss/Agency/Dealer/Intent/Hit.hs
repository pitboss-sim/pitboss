{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Agency.Dealer.Intent.Hit where

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

-- Context for validating DealerHitIntent
data DealerHitContext = DealerHitContext
    { dhcDealerHand :: SomeHand
    , dhcDealerHandFSM :: SomeDealerHandFSM
    , dhcGameRules :: GameRuleSet
    }
    deriving (Eq, Show)

-- Build context from intent
buildContext ::
    EntityId 'Intent ->
    Reader TickCacheContext (Maybe DealerHitContext)
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
                    in pure $ Just DealerHitContext
                        { dhcDealerHand = dh ^. dhAttrs . dhAttrsHand
                        , dhcDealerHandFSM = dh ^. dhModes . dhModesDealerHand
                        , dhcGameRules = gameRuleSet offering
                        }
                _ -> pure Nothing
        Nothing -> pure Nothing

-- Validate the intent
validate :: DealerHitContext -> Either String ()
validate ctx = do
    checkDealerMustHit ctx
    checkInDealingPhase ctx

checkDealerMustHit :: DealerHitContext -> Either String ()
checkDealerMustHit ctx =
    if dealerShouldHit (dhcGameRules ctx) (dhcDealerHand ctx)
        then Right ()
        else Left "Dealer should not hit with this hand"

checkInDealingPhase :: DealerHitContext -> Either String ()
checkInDealingPhase ctx = case dhcDealerHandFSM ctx of
    SomeDealerHandFSM DealingFSM -> Right ()
    _ -> Left "Dealer hand not in dealing phase"
