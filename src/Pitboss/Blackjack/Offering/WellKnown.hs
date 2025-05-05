{-# LANGUAGE DataKinds #-}

module Pitboss.Blackjack.Offering.WellKnown
  ( vegas6,
    downtownSingleDeck,
  )
where

import Pitboss.Blackjack.Offering (Offering, mkOffering)
import Pitboss.Blackjack.Offering.Matter (Dealt (..), DeckCount (..), Matter (..))
import Pitboss.Blackjack.Offering.RuleSet (DASRule (..), DoubleRule (..), Payout (..), Pen (..), ResplitAcesAllowed (NoResplitAces, ResplitAces), RuleSet (..), Soft17Rule (..), SplitAcesAllowed (..), SplitAcesFrozen (..), SplitHands (..), Surrender (..))

-- | Standard 6-deck Vegas shoe game
vegas6 :: Offering
vegas6 =
  mkOffering matter ruleSet
  where
    matter =
      Matter
        { matterDecks = D6,
          matterDealt = FaceUp
        }
    ruleSet =
      RuleSet
        { soft17 = StandSoft17,
          das = DAS,
          doubling = DoubleAny,
          splitAcesAllowed = SplitAces,
          resplitAcesAllowed = ResplitAces,
          splitAcesFrozen = FullPlay,
          splitHands = SP4,
          surrender = Late,
          payout = P3_2,
          pen = PenFrac 5 6
        }

-- | Single-deck downtown pitch game (face-down!)
downtownSingleDeck :: Offering
downtownSingleDeck =
  mkOffering matter ruleSet
  where
    matter =
      Matter
        { matterDecks = D1,
          matterDealt = Pitch
        }

    ruleSet =
      RuleSet
        { soft17 = HitSoft17,
          das = NoDAS,
          doubling = Double10_11,
          splitAcesAllowed = NoSplitAces,
          resplitAcesAllowed = NoResplitAces,
          splitAcesFrozen = OneCardOnly,
          splitHands = SP2,
          surrender = NoSurrender,
          payout = P6_5,
          pen = PenCards 50
        }
