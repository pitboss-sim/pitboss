{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Sim.Types.Identifier where

import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import System.Random (StdGen, randomR)

newtype Uid = Uid {unUid :: String}
  deriving stock (Eq, Ord, Show)

newtype PlayerId = PlayerId Uid
  deriving stock (Eq, Ord)

instance Show PlayerId where
  show (PlayerId (Uid u)) = "P" ++ u

newtype SpotId = SpotId Uid
  deriving stock (Eq, Ord)

instance Show SpotId where
  show (SpotId (Uid u)) = "O" ++ u

newtype HandId = HandId Uid
  deriving stock (Eq, Ord)

instance Show HandId where
  show (HandId (Uid u)) = "H" ++ u

newtype ShoeId = ShoeId Uid
  deriving stock (Eq, Ord)

instance Show ShoeId where
  show (ShoeId (Uid u)) = "S" ++ u

newtype RegistryId = RegistryId Uid
  deriving stock (Eq, Ord, Show)

-- Lexicographically sortable identifier
generateUid :: Int -> StdGen -> (Uid, StdGen)
generateUid counter gen =
  let (entropy, gen') = randomR (0, 2 ^ (40 :: Int) - 1) gen
      tsPart = showPaddedBase32 counter 6
      rndPart = showPaddedBase32 entropy 8
   in (Uid (tsPart ++ "-" ++ rndPart), gen')

-- Crockford base32, lexicographically sortable
showPaddedBase32 :: Int -> Int -> String
showPaddedBase32 n width = padLeft '0' width (showIntAtBase 32 intToDigit n "")

padLeft :: Char -> Int -> String -> String
padLeft c width str = replicate (width - length str) c ++ str
