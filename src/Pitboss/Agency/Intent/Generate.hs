{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Agency.Intent.Generate (
    generateIntent,
    generatePlayerIntent,
    generateDealerIntent,
    buildGameContextFromIntent,
) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Pitboss.Agency.Archetype.Dealer.Rules
import Pitboss.Agency.Archetype.Player.Advantage
import Pitboss.Agency.Archetype.Player.BasicStrategy
import Pitboss.Agency.Archetype.Player.Perfect
import Pitboss.Agency.Archetype.Player.Superstitious
import Pitboss.Agency.Archetype.Types
import Pitboss.Agency.Intent.Types
import Pitboss.Agency.Types
import Pitboss.Blackjack.Materia.Card
import Pitboss.Blackjack.Materia.Hand
import Pitboss.Blackjack.Offering qualified as O
import Pitboss.Blackjack.Play
import Pitboss.State.Entity.Lenses
import Pitboss.State.Entity.Types
import Pitboss.State.TickCache
import Pitboss.State.Types.Core
import System.Random
import Prelude hiding (round)

generateIntent :: OriginatingEntity -> GameContext -> State StdGen IntentKind
generateIntent (FromPlayer _ archetype) ctx = generatePlayerIntent archetype ctx
generateIntent (FromDealer _ archetype) ctx = generateDealerIntent archetype ctx
generateIntent (FromTable _) _ = pure IDealerDeal

generatePlayerIntent :: SomePlayerArchetype -> GameContext -> State StdGen IntentKind
generatePlayerIntent archetype ctx = do
    move <- case archetype of
        SomePlayerBasicStrategy (BasicStrategyArchetype config _) ->
            getBasicStrategyMove config ctx
        SomePlayerPerfect (PerfectArchetype config _) ->
            getPerfectMove config ctx
        SomePlayerAdvantage (AdvantageArchetype config state') ->
            getAdvantageMove config state' ctx
        SomePlayerSuperstitious (SuperstitiousArchetype config _) ->
            getSuperstitiousMove config ctx
    pure $ moveToIntentKind move

generateDealerIntent :: SomeDealerArchetype -> GameContext -> State StdGen IntentKind
generateDealerIntent _ ctx =
    let dealerHand = _contextDealerUpcard ctx
    in case determineRequiredDealerAction ctx (characterize [dealerHand]) of
        MustHit -> pure IDealerHit
        MustStand -> pure IDealerStand
        MustDeal -> pure IDealerDeal
        MustSettle -> pure IDealerSettleBout

moveToIntentKind :: Move -> IntentKind
moveToIntentKind Hit = IPlayerHit
moveToIntentKind Stand = IPlayerStand
moveToIntentKind Double = IPlayerDouble
moveToIntentKind Split = IPlayerSplit
moveToIntentKind Surrender = IPlayerSurrender

buildGameContext ::
    SomeHand ->
    Card ->
    O.Offering ->
    Int ->
    Double ->
    GameContext
buildGameContext hand dealerUpcard offering handNumber _decksRemaining =
    GameContext
        { _contextPlayerHand = hand
        , _contextDealerUpcard = dealerUpcard
        , _contextOffering = offering
        , _contextCanDouble = canDoubleSomeHand hand offering
        , _contextCanSplit = canSplitSomeHand hand handNumber offering
        , _contextCanSurrender = True
        , _contextHandNumber = handNumber
        , _contextSplitCount = 0
        }

buildGameContextFromIntent ::
    EntityId 'Intent ->
    Reader TickCacheContext (Maybe GameContext)
buildGameContextFromIntent intentId = do
    intent <- deref intentId
    case intent of
        Just (EIntent _ _ rels) -> do
            bout <- maybe (pure Nothing) deref (_intentRelsTargetBout rels)
            dealerHand <- case bout of
                Just b -> deref (b ^. bRels . bRelsDealerHand)
                Nothing -> pure Nothing
            playerHand <- case bout of
                Just b -> deref (b ^. bRels . bRelsPlayerHand)
                Nothing -> pure Nothing

            let dealerUpcard = case dealerHand of
                    Just dh ->
                        let hand = dh ^. dhAttrs . dhAttrsHand
                        in case firstUpcardFromHand hand of
                            Just card -> card
                            Nothing -> Card Ace Spades
                    Nothing -> Card Ace Spades

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

            case (playerHand, table) of
                (Just ph, Just t) ->
                    let hand = ph ^. phAttrs . phAttrsHand
                        offering = t ^. tAttrs . tAttrsOffering
                    in pure $ Just $ buildGameContext hand dealerUpcard offering 0 6.0
                _ -> pure Nothing
        _ -> pure Nothing
  where
    firstUpcardFromHand _ = Just (Card Ace Spades)
