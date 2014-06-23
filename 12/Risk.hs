{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
    deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
    random           = first DV . randomR (1,6)
    randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
    let attacks = (attackers bf) - 1 -- attacking army has to leave behind one soldier
        defends = defenders bf
    attackDieRolls <- replicateM attacks die -- simulate x number of die rolls
    defendDieRolls <- replicateM defends die
    let a = reverse $ sort attackDieRolls -- sorted in ascending order, so reverse
        b = reverse $ sort defendDieRolls
        pairs = zip a b
        attackersLeft = length $ filter (uncurry (>)) pairs -- attacker wins if higher die value
        defendersLeft = length $ filter (uncurry (>=)) pairs -- defender wins if higher or equal value
    return (Battlefield (attackersLeft + 1) (defends - attacks + defendersLeft))


-- Simulates an entire invasion attempt, that is, repeated calls
-- to battle until there are no defenders remaining, or fewer than two attackers
invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
    if defenders bf == 0 || attackers bf < 2 -- if conditions are met, just return the battlefield
    then return bf
    else battle bf >>= invade -- otherwise keep battling


attackerWins :: Battlefield -> Double
attackerWins bf =
    if (defenders bf == 0 && attackers bf > 1) -- attacker wins if defending army has 0 units and attacking army has > 1 unit 
    then 1.0
    else 0.0

-- Runs invade 1000 times, and uses the results to compute a Double 
-- between 0 and 1 representing the estimated probability that
-- the attacking army will completely destroy the defending army.
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
    battles <- replicateM 1000 (invade bf) -- simulate 1000 invasions
    let wins = foldl1 (+) (map (attackerWins) battles)
    return (wins / 1000.0)


