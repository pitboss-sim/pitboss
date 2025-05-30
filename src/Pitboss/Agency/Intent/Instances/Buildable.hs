{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Agency.Intent.Instances.Buildable where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Pitboss.Agency.Intent.Types
import Pitboss.Blackjack.Materia.Card
import Pitboss.Blackjack.Offering (gameRuleSet)
import Pitboss.FSM.DealerHand (DealerHandFSM (..), SomeDealerHandFSM (..))
import Pitboss.FSM.PlayerHand (PlayerHandFSM (..), SomePlayerHandFSM (..))
import Pitboss.State.Entity.Lenses
import Pitboss.State.Entity.Types
import Pitboss.State.TickCache (TickCacheContext, deref)
import Pitboss.State.Types.Core
import Prelude hiding (round)

class Buildable (k :: IntentKind) where
    buildICtx :: EntityId 'Intent -> Reader TickCacheContext (Maybe (IntentCtx k))

instance Buildable 'IPlayerHit where
    buildICtx intentId = do
        intent <- deref intentId
        case intent of
            Just (EIntent _ _ rels) -> do
                bout <- maybe (pure Nothing) deref (_intentRelsTargetBout rels)

                playerHand <- case bout of
                    Just b -> deref (b ^. bRels . bRelsPlayerHand)
                    Nothing -> pure Nothing

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
                    Just ph ->
                        pure $
                            Just
                                PlayerHitCtx
                                    { phICtxPlayerHand = ph ^. phAttrs . phAttrsHand
                                    , phICtxPlayerHandFSM = ph ^. phModes . phFsm
                                    , phICtxIsPlayersTurn = True
                                    , phICtxShoeHasCards = shoeHasCards
                                    }
                    Nothing -> pure Nothing
            Nothing -> pure Nothing
      where
        hasAvailableCards :: Map CardIx CardState -> Bool
        hasAvailableCards cardStates =
            any notDealt (Map.elems cardStates)
          where
            notDealt InHand = False
            notDealt InDiscard = False
            notDealt Burned = False

instance Buildable 'IPlayerStand where
    buildICtx intentId = do
        intent <- deref intentId
        case intent of
            Just (EIntent _ _ rels) -> do
                bout <- maybe (pure Nothing) deref (_intentRelsTargetBout rels)

                playerHand <- case bout of
                    Just b -> deref (b ^. bRels . bRelsPlayerHand)
                    Nothing -> pure Nothing

                case playerHand of
                    Just ph ->
                        pure $
                            Just
                                PlayerStandCtx
                                    { psICtxPlayerHand = ph ^. phAttrs . phAttrsHand
                                    , psICtxPlayerHandFSM = ph ^. phModes . phFsm
                                    , psICtxIsPlayersTurn = True
                                    }
                    Nothing -> pure Nothing
            Nothing -> pure Nothing

instance Buildable 'IPlayerDouble where
    buildICtx intentId = do
        intent <- deref intentId
        case intent of
            Just (EIntent _ _ rels) -> do
                bout <- maybe (pure Nothing) deref (_intentRelsTargetBout rels)

                playerHand <- case bout of
                    Just b -> deref (b ^. bRels . bRelsPlayerHand)
                    Nothing -> pure Nothing

                player <- case playerHand of
                    Just ph -> do
                        spot <- deref (ph ^. phRels . phRelsBelongsToPlayerSpot)
                        case spot of
                            Just s -> deref (s ^. psRels . psEntityRelsPlayerId)
                            Nothing -> pure Nothing
                    Nothing -> pure Nothing

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
                        pure $
                            Just
                                PlayerDoubleCtx
                                    { pdICtxPlayerHand = ph ^. phAttrs . phAttrsHand
                                    , pdICtxPlayerHandFSM = ph ^. phModes . phFsm
                                    , pdICtxIsPlayersTurn = True
                                    , pdICtxOffering = t ^. tAttrs . tAttrsOffering
                                    , pdICtxPlayerBankroll = p ^. pAttrs . pAttrsBankroll
                                    , pdICtxCurrentBet = ph ^. phAttrs . phAttrsOriginalBet
                                    }
                    _ -> pure Nothing
            Nothing -> pure Nothing

instance Buildable 'IPlayerSplit where
    buildICtx _ = pure Nothing

instance Buildable 'IPlayerSurrender where
    buildICtx _ = pure Nothing

instance Buildable 'IDealerHit where
    buildICtx intentId = do
        intent <- deref intentId
        case intent of
            Just (EIntent _ _ rels) -> do
                bout <- maybe (pure Nothing) deref (_intentRelsTargetBout rels)

                dealerHand <- case bout of
                    Just b -> deref (b ^. bRels . bRelsDealerHand)
                    Nothing -> pure Nothing

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
                         in pure $
                                Just
                                    DealerHitCtx
                                        { dhICtxDealerHand = dh ^. dhAttrs . dhAttrsHand
                                        , dhICtxDealerHandFSM = dh ^. dhModes . dhModesDealerHand
                                        , dhICtxGameRules = gameRuleSet offering
                                        }
                    _ -> pure Nothing
            Nothing -> pure Nothing

instance Buildable 'IDealerStand where
    buildICtx intentId = do
        intent <- deref intentId
        case intent of
            Just (EIntent _ _ rels) -> do
                bout <- maybe (pure Nothing) deref (_intentRelsTargetBout rels)

                dealerHand <- case bout of
                    Just b -> deref (b ^. bRels . bRelsDealerHand)
                    Nothing -> pure Nothing

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
                         in pure $
                                Just
                                    DealerStandCtx
                                        { dsICtxDealerHand = dh ^. dhAttrs . dhAttrsHand
                                        , dsICtxDealerHandFSM = dh ^. dhModes . dhModesDealerHand
                                        , dsICtxGameRules = gameRuleSet offering
                                        }
                    _ -> pure Nothing
            Nothing -> pure Nothing

instance Buildable 'IDealerDeal where
    buildICtx intentId = do
        intent <- deref intentId
        case intent of
            Just (EIntent attrs _ rels) ->
                case _intentAttrsDetails attrs of
                    TableDealCardIntent targetHandId -> do
                        bout <- maybe (pure Nothing) deref (_intentRelsTargetBout rels)

                        round <- case bout of
                            Just b -> deref (b ^. bRels . bRelsDealerRound)
                            Nothing -> pure Nothing

                        shoe <- case round of
                            Just r -> deref (r ^. drRels . drRelsTableShoeUsed)
                            Nothing -> pure Nothing

                        let cardsRemaining = case shoe of
                                Just s -> countAvailableCards (s ^. tsAttrs . tsAttrsCardStates)
                                Nothing -> 0

                        targetFSM <- getTargetHandFSM (Left targetHandId)

                        pure $
                            Just
                                DealerDealCtx
                                    { ddICtxTargetHand = Left targetHandId
                                    , ddICtxTargetHandFSM = targetFSM
                                    , ddICtxShoeCardsRemaining = cardsRemaining
                                    , ddICtxRoundPhase = Nothing
                                    , ddICtxNextCard = Card Ace Spades
                                    }
                    _ -> pure Nothing
            Nothing -> pure Nothing
      where
        countAvailableCards :: Map CardIx CardState -> Int
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
                Nothing -> Left (SomePlayerHandFSM DecisionFSM)
        getTargetHandFSM (Right dealerId) = do
            hand <- deref dealerId
            pure $ case hand of
                Just h -> Right (h ^. dhModes . dhModesDealerHand)
                Nothing -> Right (SomeDealerHandFSM DealingFSM)

instance Buildable 'IDealerSettleBout where
    buildICtx _ = pure Nothing

instance Buildable 'IDealerSettleInsurance where
    buildICtx _ = pure Nothing
