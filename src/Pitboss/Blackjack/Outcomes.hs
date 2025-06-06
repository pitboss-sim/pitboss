{-# LANGUAGE DeriveGeneric #-}

module Pitboss.Blackjack.Outcomes where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data BoutOutcome
    = PlayerWins
    | DealerWins
    | Push
    deriving (Eq, Show, Generic)

data WinReason
    = HigherScore
    | OpponentBust
    | NaturalBlackjack
    deriving (Eq, Show, Generic)

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

basicOutcome :: DetailedOutcome -> BoutOutcome
basicOutcome = outcome
