{-# LANGUAGE DataKinds #-}

module Pitboss.TestHelpers where

import Control.Exception (SomeException, handle)
import Data.Text.IO qualified as TIO
import Pitboss.Blackjack
import Pitboss.Blackjack.BasicStrategy.Chart
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation.Agency.Archetype.Types

loadCanonicalStrategy :: IO StrategyChart
loadCanonicalStrategy = handle handleError $ do
    baseline <- TIO.readFile "data/strategy/baseline.txt"
    overlay <- TIO.readFile "data/strategy/bja-basic.txt"
    case (parseStrategyChart baseline, parseStrategyChart overlay) of
        (Right baseChart, Right overlayChart) ->
            pure $ overlayStrategy baseChart overlayChart
        (Left errs, _) ->
            error $ "Failed to parse baseline strategy: " ++ show errs
        (_, Left errs) ->
            error $ "Failed to parse overlay strategy: " ++ show errs
  where
    handleError :: SomeException -> IO StrategyChart
    handleError ex = error $ "Failed to load strategy files: " ++ show ex

workingMistakeDistribution :: MistakeDistribution
workingMistakeDistribution =
    MistakeDistribution
        { _hitInsteadOfStand = 0.0
        , _standInsteadOfHit = 0.0
        , _noDoubleWhenShould = 0.0
        , _noSplitWhenShould = 0.0
        , _doubleWhenShouldnt = 0.0
        , _splitWhenShouldnt = 0.0
        }

mkTestBasicStrategy :: IO SomePlayerArchetype
mkTestBasicStrategy = do
    chart <- loadCanonicalStrategy
    pure $
        SomePlayerBasicStrategy $
            BasicStrategyArchetype
                { bsConfig = BasicConfig chart (MistakeProfile 0.0 workingMistakeDistribution)
                , bsState = BasicState 0 emptySessionStats
                }

mkTestDealerArchetype :: SomeDealerArchetype
mkTestDealerArchetype =
    SomeDealerByTheBook $
        ByTheBookDealerArchetype
            { btbConfig = ByTheBookConfig (PenetrationProfile 0.75 0.05) (PaceProfile 100 10.0)
            , btbState = ByTheBookState 0
            }

mkTestPlayer :: EntityId 'Player -> String -> IO (EntityState 'Player)
mkTestPlayer _playerId name = do
    pure $
        EPlayer
            { _pAttrs =
                PlayerAttrs
                    { _pAttrsName = name
                    , _pAttrsBankroll = Chips 1000
                    }
            , _pModes =
                PlayerModes
                    { _pModesPlayerTable = SomePlayerTableFSM PTIdleFSM
                    , _pModesPlayerSpot = SomePlayerSpotFSM PSIdleFSM
                    , _pModesPlayerHand = SomePlayerHandFSM PHDecisionFSM
                    }
            , _pRels = PlayerRels
            }

mkTestDealer :: EntityId 'Dealer -> String -> EntityState 'Dealer
mkTestDealer _dealerId name =
    EDealer
        { _dAttrs =
            DealerAttrs
                { _dAttrsName = name
                -- , _dAttrsArchetype = mkTestDealerArchetype
                }
        , _dModes =
            DealerModes
                { _dModesDealerTable = SomeDealerTableFSM DTOffDutyFSM
                , _dModesDealerRound = PeekDealerRound (SomePeekFSM PeekAwaitingFSM)
                , _dModesDealerHand = SomeDealerHandFSM DHDealingFSM
                }
        , _dRels = DealerRels Nothing Nothing Nothing
        }

mkTestPlayerHand ::
    EntityId 'PlayerHand ->
    EntityId 'PlayerSpot ->
    EntityId 'DealerRound ->
    EntityId 'Player ->
    EntityId 'Bout ->
    EntityState 'PlayerHand
mkTestPlayerHand _handId spotId roundId playerId boutId =
    EPlayerHand
        { _phAttrs =
            PlayerHandAttrs
                { _phAttrsHand = characterize []
                , _phAttrsOriginalBet = Chips 100
                , _phAttrsSplitDepth = 0
                , _phAttrsHandIx = 0
                }
        , _phModes = PlayerHandModes (SomePlayerHandFSM PHDecisionFSM)
        , _phRels = PlayerHandRels spotId roundId playerId boutId
        }

mkTestDealerHand :: EntityId 'DealerHand -> EntityId 'DealerRound -> EntityId 'Dealer -> EntityState 'DealerHand
mkTestDealerHand _handId roundId dealerId =
    EDealerHand
        { _dhAttrs = DealerHandAttrs (characterize [])
        , _dhModes = DealerHandModes (SomeDealerHandFSM DHDealingFSM)
        , _dhRels = DealerHandRels roundId dealerId
        }

mkTestBout :: EntityId 'Bout -> EntityId 'PlayerHand -> EntityId 'DealerHand -> EntityId 'TableShoe -> EntityId 'Table -> EntityId 'DealerRound -> EntityState 'Bout
mkTestBout _boutId playerHandId dealerHandId shoeId tableId roundId =
    EBout
        { _boutAttrs = BoutAttrs Nothing
        , _boutModes = BoutModes (SomeBoutFSM BAwaitingFirstCardFSM)
        , _boutRels = BoutRels playerHandId dealerHandId shoeId tableId roundId
        }

createPlayerHandForBout ::
    EntityId 'PlayerHand ->
    EntityId 'PlayerSpot ->
    EntityId 'DealerRound ->
    EntityId 'Player ->
    EntityId 'Bout ->
    Chips ->
    EntityState 'PlayerHand
createPlayerHandForBout _handId spotId roundId playerId boutId wager =
    EPlayerHand
        { _phAttrs =
            PlayerHandAttrs
                { _phAttrsHand = characterize []
                , _phAttrsOriginalBet = wager
                , _phAttrsSplitDepth = 0
                , _phAttrsHandIx = 0
                }
        , _phModes = PlayerHandModes (SomePlayerHandFSM PHDecisionFSM)
        , _phRels =
            PlayerHandRels
                { _phRelsBelongsToPlayerSpot = spotId
                , _phRelsBelongsToDealerRound = roundId
                , _phRelsOwnedByPlayer = playerId
                , _phRelsBelongsToBout = boutId
                }
        }
