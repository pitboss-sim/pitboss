{-# LANGUAGE DeriveGeneric #-}

-- File: src/Pitboss/Blackjack/Outcome.hs
module Pitboss.Blackjack.Outcome where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | Core outcome of a bout - who won or push
data BoutOutcome
    = PlayerWins
    | DealerWins
    | Push
    deriving (Eq, Show, Generic)

-- | Why someone won (if we care about the details)
data WinReason
    = HigherScore
    | OpponentBust
    | NaturalBlackjack
    deriving (Eq, Show, Generic)

-- | Detailed outcome tracking both result and reason
data DetailedOutcome = DetailedOutcome
    { outcome :: BoutOutcome
    , reason :: Maybe WinReason
    }
    deriving (Eq, Show, Generic)

instance ToJSON BoutOutcome
instance FromJSON BoutOutcome
instance ToJSON WinReason
instance FromJSON WinReason
instance ToJSON DetailedOutcome
instance FromJSON DetailedOutcome

-- Helper constructors for common cases
playerWinsHigher :: DetailedOutcome
playerWinsHigher = DetailedOutcome PlayerWins (Just HigherScore)

playerWinsDealerBust :: DetailedOutcome
playerWinsDealerBust = DetailedOutcome PlayerWins (Just OpponentBust)

playerWinsBlackjack :: DetailedOutcome
playerWinsBlackjack = DetailedOutcome PlayerWins (Just NaturalBlackjack)

dealerWinsHigher :: DetailedOutcome
dealerWinsHigher = DetailedOutcome DealerWins (Just HigherScore)

dealerWinsPlayerBust :: DetailedOutcome
dealerWinsPlayerBust = DetailedOutcome DealerWins (Just OpponentBust)

dealerWinsBlackjack :: DetailedOutcome
dealerWinsBlackjack = DetailedOutcome DealerWins (Just NaturalBlackjack)

pushOutcome :: DetailedOutcome
pushOutcome = DetailedOutcome Push Nothing

-- | Extract just the basic outcome (for events, etc)
basicOutcome :: DetailedOutcome -> BoutOutcome
basicOutcome = outcome
