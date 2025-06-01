{-# LANGUAGE DataKinds #-}

module Pitboss.Agency.Intent.Generate (
    generateIntentFromContext,
    buildGameContextFromIntent,
) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Pitboss.Agency.Archetype.Player.Strategy.Types
import Pitboss.Agency.Archetype.Types
import Pitboss.Agency.Intent.Generate.Strategy
import Pitboss.Agency.Intent.Types
import Pitboss.Blackjack.Materia.Card
import Pitboss.State.Entity.Lenses
import Pitboss.State.Entity.Types
import Pitboss.State.TickCache
import Pitboss.State.Types.Core
import System.Random
import Prelude hiding (round)

generateIntentFromContext ::
    SomePlayerArchetype ->
    GameContext ->
    State StdGen IntentKind
generateIntentFromContext = generatePlayerIntent

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
    firstUpcardFromHand _ =
        Just (Card Ace Spades) -- Simplified for now
