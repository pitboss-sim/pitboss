{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Pitboss.Agency.Dealer.Intent.DealCard where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Map.Strict qualified as Map
import Pitboss.FSM.DealerHand (SomeDealerHandFSM (..), DealerHandFSM (..))
import Pitboss.FSM.DealerRound (DealerRoundFSM)
import Pitboss.FSM.PlayerHand (SomePlayerHandFSM (..), isHandTerminal, PlayerHandFSM (..))
import Pitboss.State.Entity.Lenses
import Pitboss.State.Entity.Types
import Pitboss.State.TickCache (TickCacheContext, deref)
import Pitboss.State.Types.Core
import Prelude hiding (round)

-- Context for validating DealCardIntent
data DealCardContext = DealCardContext
    { dccTargetHand :: Either (EntityId 'PlayerHand) (EntityId 'DealerHand)
    , dccTargetHandFSM :: Either SomePlayerHandFSM SomeDealerHandFSM
    , dccShoeCardsRemaining :: Int
    , dccRoundPhase :: Maybe DealerRoundFSM -- TODO: add round FSM to entities
    }
    deriving (Eq, Show)

-- Build context from intent
buildContext ::
    EntityId 'Intent ->
    Reader TickCacheContext (Maybe DealCardContext)
buildContext intentId = do
    intent <- deref intentId
    case intent of
        Just (EIntent attrs _ rels) ->
            case _intentAttrsDetails attrs of
                TableDealCardIntent targetHandId -> do
                    -- Get bout from intent
                    bout <- maybe (pure Nothing) deref (_intentRelsTargetBout rels)

                    -- Get round from bout
                    round <- case bout of
                        Just b -> deref (b ^. bRels . bRelsDealerRound)
                        Nothing -> pure Nothing

                    -- Get shoe from round
                    shoe <- case round of
                        Just r -> deref (r ^. drRels . drRelsTableShoeUsed)
                        Nothing -> pure Nothing

                    -- Count available cards
                    let cardsRemaining = case shoe of
                            Just s -> countAvailableCards (s ^. tsAttrs . tsAttrsCardStates)
                            Nothing -> 0

                    -- Get target hand FSM
                    targetFSM <- getTargetHandFSM (Left targetHandId)

                    pure $ Just DealCardContext
                        { dccTargetHand = Left targetHandId -- TODO: handle dealer hands
                        , dccTargetHandFSM = targetFSM
                        , dccShoeCardsRemaining = cardsRemaining
                        , dccRoundPhase = Nothing -- TODO: get from entity
                        }
                _ -> pure Nothing
        Nothing -> pure Nothing

-- Validate the intent
validate :: DealCardContext -> Either String ()
validate ctx = do
    checkShoeHasCards ctx
    checkTargetCanReceiveCard ctx
    checkRoundPhaseAllowsDealing ctx

checkShoeHasCards :: DealCardContext -> Either String ()
checkShoeHasCards ctx =
    if dccShoeCardsRemaining ctx > 0
        then Right ()
        else Left "No cards remaining in shoe"

checkTargetCanReceiveCard :: DealCardContext -> Either String ()
checkTargetCanReceiveCard ctx = case dccTargetHandFSM ctx of
    Left playerFSM ->
        if not (isHandTerminal playerFSM)
            then Right ()
            else Left "Player hand cannot receive card - already complete"
    Right _dealerFSM ->
        Right () -- TODO: check dealer FSM state

checkRoundPhaseAllowsDealing :: DealCardContext -> Either String ()
checkRoundPhaseAllowsDealing _ = Right () -- TODO: implement when round FSM available

-- Helper functions
countAvailableCards :: Map.Map CardIx CardState -> Int
countAvailableCards cardStates =
    Map.size $ Map.filter notDealt cardStates
  where
    notDealt InHand = False
    notDealt InDiscard = False
    notDealt Burned = False

getTargetHandFSM ::
    Either (EntityId 'PlayerHand) (EntityId 'DealerHand) ->
    Reader TickCacheContext (Either SomePlayerHandFSM SomeDealerHandFSM)
getTargetHandFSM (Left playerId) = do
    hand <- deref playerId
    pure $ case hand of
        Just h -> Left (h ^. phModes . phFsm)
        Nothing -> Left (SomePlayerHandFSM DecisionFSM) -- TODO: handle properly
getTargetHandFSM (Right dealerId) = do
    hand <- deref dealerId
    pure $ case hand of
        Just h -> Right (h ^. dhModes . dhModesDealerHand)
        Nothing -> Right (SomeDealerHandFSM DealingFSM) -- TODO: handle properly
