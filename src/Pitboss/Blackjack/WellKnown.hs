{-# LANGUAGE DataKinds #-}

module Pitboss.Blackjack.WellKnown where

import Pitboss.Blackjack.Rules.Game
import Pitboss.Blackjack.Rules.Offering
import Pitboss.Blackjack.Rules.Table

vegas6 :: Offering
vegas6 =
    mkOffering matter gameRuleSet tableRuleSet
  where
    matter =
        Materia
            { matterDecks = D6
            , matterDealt = FaceUp
            }
    gameRuleSet =
        GameRuleSet
            { soft17 = StandSoft17
            , das = DAS
            , doubling = DoubleAny
            , splitAcesAllowed = SplitAces
            , resplitAcesAllowed = ResplitAces
            , splitAcesFrozen = FullPlay
            , splitHands = SP4
            , surrender = Late
            , payout = P3_2
            , pen = PenFrac 5 6
            , holeCardRule = Peek
            }
    tableRuleSet =
        TableRuleSet
            { minBet = 25
            , maxBet = 5000
            , maxSpotsPerPlayer = 2
            , midShoeEntry = AllowMidShoe
            , multiSpotMinBetPolicy = MultipliedMinBet 2.0
            , burnPolicy = SingleCardBurn
            , standardPenetration = 0.75
            }

downtownSingleDeck :: Offering
downtownSingleDeck =
    mkOffering matter gameRuleSet tableRuleSet
  where
    matter =
        Materia
            { matterDecks = D1
            , matterDealt = Pitch
            }
    gameRuleSet =
        GameRuleSet
            { soft17 = HitSoft17
            , das = NoDAS
            , doubling = Double10_11
            , splitAcesAllowed = NoSplitAces
            , resplitAcesAllowed = NoResplitAces
            , splitAcesFrozen = OneCardOnly
            , splitHands = SP2
            , surrender = NoSurrender
            , payout = P6_5
            , pen = PenCards 50
            , holeCardRule = Peek
            }
    tableRuleSet =
        TableRuleSet
            { minBet = 5
            , maxBet = 500
            , maxSpotsPerPlayer = 1
            , midShoeEntry = NoMidShoe
            , multiSpotMinBetPolicy = SameMinBet
            , burnPolicy = NoBurn
            , standardPenetration = 0.85
            }
