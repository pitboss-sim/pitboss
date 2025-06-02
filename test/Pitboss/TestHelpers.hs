{-# LANGUAGE DataKinds #-}

module Pitboss.TestHelpers where

import Pitboss.Agency.Archetype.Types
import Pitboss.Blackjack.BasicStrategy.Chart.Types (StrategyChart)
import Pitboss.Blackjack.Materia.Chips
import Pitboss.Blackjack.Materia.Hand
import Pitboss.Blackjack.Offering.WellKnown (vegas6)
import Pitboss.FSM.Bout
import Pitboss.FSM.DealerHand
import Pitboss.FSM.DealerRound
import Pitboss.FSM.DealerTable
import Pitboss.FSM.PlayerHand
import Pitboss.FSM.PlayerSpot
import Pitboss.FSM.PlayerTable
import Pitboss.State.Entity.Types
import Pitboss.State.Types.Core

-- Helper to create minimal valid archetype configurations
mkTestBasicStrategy :: SomePlayerArchetype
mkTestBasicStrategy =
    SomePlayerBasicStrategy $
        BasicStrategyArchetype
            { bsConfig = BasicConfig emptyChart (MistakeProfile 0.0 emptyMistakeDistribution)
            , bsState = BasicState 0 emptySessionStats
            }

mkTestDealerArchetype :: SomeDealerArchetype
mkTestDealerArchetype =
    SomeDealerByTheBook $
        ByTheBookDealerArchetype
            { btbConfig = ByTheBookConfig (PenetrationProfile 0.75 0.05) (PaceProfile 100 10.0)
            , btbState = ByTheBookState 0
            }

-- Helper empty/default values to avoid undefined
emptyChart :: StrategyChart
emptyChart = []

emptyMistakeDistribution :: MistakeDistribution
emptyMistakeDistribution = MistakeDistribution 0 0 0 0 0 0

-- Helper to create test entities
mkTestPlayer :: EntityId 'Player -> String -> EntityState 'Player
mkTestPlayer playerId name =
    EPlayer
        { _pAttrs =
            PlayerAttrs
                { _pAttrsName = name
                , _pAttrsBankroll = Chips 1000
                , _pAttrsArchetype = mkTestBasicStrategy
                }
        , _pModes =
            PlayerModes
                { _pModesPlayerTable = SomePlayerTableFSM IdleFSM
                , _pModesPlayerSpot = SomePlayerSpotFSM SpotIdleFSM
                , _pModesPlayerHand = SomePlayerHandFSM DecisionFSM
                }
        , _pRels = PlayerRels
        }

mkTestDealer :: EntityId 'Dealer -> String -> EntityState 'Dealer
mkTestDealer dealerId name =
    EDealer
        { _dAttrs =
            DealerAttrs
                { _dAttrsName = name
                , _dAttrsArchetype = mkTestDealerArchetype
                }
        , _dModes =
            DealerModes
                { _dModesDealerTable = SomeDealerTableFSM OffDutyFSM
                , _dModesDealerRound = PeekDealerRound (SomePeekFSM PeekAwaitingFSM)
                , _dModesDealerHand = SomeDealerHandFSM DealingFSM
                }
        , _dRels = DealerRels Nothing Nothing Nothing
        }

mkTestPlayerHand :: EntityId 'PlayerHand -> EntityId 'PlayerSpot -> EntityId 'DealerRound -> EntityId 'Player -> EntityState 'PlayerHand
mkTestPlayerHand handId spotId roundId playerId =
    EPlayerHand
        { _phAttrs =
            PlayerHandAttrs
                { _phAttrsHand = characterize []
                , _phAttrsOriginalBet = Chips 100
                , _phAttrsSplitDepth = 0
                , _phAttrsHandIx = 0
                }
        , _phModes = PlayerHandModes (SomePlayerHandFSM DecisionFSM)
        , _phRels = PlayerHandRels spotId roundId playerId
        }

mkTestDealerHand :: EntityId 'DealerHand -> EntityId 'DealerRound -> EntityId 'Dealer -> EntityState 'DealerHand
mkTestDealerHand handId roundId dealerId =
    EDealerHand
        { _dhAttrs = DealerHandAttrs (characterize [])
        , _dhModes = DealerHandModes (SomeDealerHandFSM DealingFSM)
        , _dhRels = DealerHandRels roundId dealerId
        }

mkTestBout :: EntityId 'Bout -> EntityId 'PlayerHand -> EntityId 'DealerHand -> EntityId 'TableShoe -> EntityId 'Table -> EntityId 'DealerRound -> EntityState 'Bout
mkTestBout boutId playerHandId dealerHandId shoeId tableId roundId =
    EBout
        { _boutAttrs = BoutAttrs Nothing
        , _boutModes = BoutModes (SomeBoutFSM AwaitingFirstCardFSM)
        , _boutRels = BoutRels playerHandId dealerHandId shoeId tableId roundId
        }
