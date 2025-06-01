{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Agency.Archetype.Example where

import Pitboss.Agency.Archetype.Types

-- | Example: Type-safe archetype dispatch
processPlayerAction :: SomePlayerArchetype -> IO ()
processPlayerAction = \case
    SomePlayerBasicStrategy arch -> handleBasicPlayer arch
    SomePlayerPerfect arch -> handlePerfectPlayer arch
    SomePlayerAdvantage arch -> handleAdvantagePlayer arch
    SomePlayerSuperstitious arch -> handleSuperstitiousPlayer arch

handleBasicPlayer :: PlayerArchetype 'BasicStrategy -> IO ()
handleBasicPlayer arch = do
    putStrLn $ "Basic player with " ++ show (bcMistakeProfile $ bsConfig arch)
    putStrLn $ "Mistakes made: " ++ show (bsMistakesMade $ bsState arch)

handlePerfectPlayer :: PlayerArchetype 'Perfect -> IO ()
handlePerfectPlayer arch = do
    putStrLn $ "Perfect player using EV: " ++ show (pcUseEV $ pfConfig arch)
    putStrLn $ "Hands played: " ++ show (psHandsPlayed $ pfState arch)

handleAdvantagePlayer :: PlayerArchetype 'Advantage -> IO ()
handleAdvantagePlayer arch = do
    putStrLn $ "Advantage player with count: " ++ show (asRunningCount $ advState arch)
    putStrLn $ "True count: " ++ show (asTrueCount $ advState arch)

handleSuperstitiousPlayer :: PlayerArchetype 'Superstitious -> IO ()
handleSuperstitiousPlayer arch = do
    putStrLn $ "Superstitious player with beliefs: " ++ show (scBeliefs $ ssConfig arch)
    putStrLn $ "Hands played: " ++ show (ssHandsPlayed $ ssState arch)

-- | Example: Using the type class
describeCapabilities :: forall k. (ArchetypeOps k) => PlayerArchetypeKind -> PlayerArchetype k -> String
describeCapabilities _ arch =
    unlines
        [ "Strategy: " ++ case getStrategy arch of
            UseChart -> "UseChart"
            UseEV -> "UseEV"
            UseCount -> "UseCount"
        , "Can count: " ++ show (canCount arch)
        , "Can make mistakes: " ++ show (canMakeMistakes arch)
        ]

{- | Example: Integration with Entity system
This shows how archetypes could integrate with your existing entity system
-}

{-
-- In Entity/Types.hs, you might add:
data instance EntityState 'Player = PlayerEntityState
  { playerArchetype :: SomePlayerArchetype
  , playerSpot :: SpotId
  , playerBankroll :: Chips
  }

-- Then you could have type-safe operations:
updatePlayerCount :: EntityId 'Player -> Int -> Registry -> Maybe Registry
updatePlayerCount playerId cardValue registry = do
  playerState <- lookupEntity playerId registry
  case playerArchetype playerState of
    SomePlayerAdvantage arch ->
      let newArch = arch { advState = (advState arch) { asRunningCount = asRunningCount (advState arch) + cardValue } }
          newState = playerState { playerArchetype = SomePlayerAdvantage newArch }
      in Just $ updateEntity playerId newState registry
    _ -> Nothing  -- Other archetypes can't count
-}

{- | Example: Dealer archetypes are already defined in Types2
Here we just show how to use them:
-}
processDealerAction :: SomeDealerArchetype -> IO ()
processDealerAction = \case
    SomeDealerByTheBook arch ->
        putStrLn $ "By-the-book dealer with penetration: " ++ show (btbPenetration $ btbConfig arch)
    SomeDealerRookie arch ->
        putStrLn $ "Rookie dealer with errors made: " ++ show (rkErrorsMade $ rkState arch)
    SomeDealerVeteran arch ->
        putStrLn $ "Veteran dealer with tells given: " ++ show (vtTellsGiven $ vtState arch)

-- | Example: Archetype progression/evolution
evolveArchetype :: SomePlayerArchetype -> Maybe SomePlayerArchetype
evolveArchetype (SomePlayerBasicStrategy arch) =
    if bsMistakesMade (bsState arch) < 10
        then Just $ mkPerfectPlayer True -- Graduate to perfect play
        else Nothing
evolveArchetype _ = Nothing -- Other archetypes don't evolve (in this example)
