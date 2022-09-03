module Modes.Newsave.Newsave where

import System.Console.ANSI
import System.TimeIt

--my libs
import Modes.Shared
import Basic
import Json.Parser
import Modes.Error



newsaveDisplay :: [String]
newsaveDisplay = ["Bombushka","Seer's Blood","Rook's Bomb","Lightning Bomb","Galoshes","Bottled Lightning","Salamander Tail","Guidance","Ursine Ring","Demon Ring","Intensifier","Cracked Orb","Conductor","Grimhilde's Mirror","Meal Ticket","Dillon's Claw","Bramble Vest","Leftovers","Spare Ordnance","Simple Chest","Unstable Concoction","Totem of Life","Golden Popcorn","Miner's Flask","Sewing Kit","Floating Skull","Float Boots","Key Blade","War Paint","Sonic Boom","Gold Frenzy","Butcher's Cleaver","Iron Branch","Knight's Pendant","Queen's Crown","Aegis","Adventurer's Whip","Axe Thrower's Pendant","Cosmic Egg","Battle Standard","Battle Axe","Tent","Masa","Lunchbox","Phantasmal Axe","Gecko Blast","Soul Cannon","Greaves","Pauldron","Obsidian Knife","Fork","Ursa Major","Canis Major","Sagitta","Circinus","Orion's Sword","Shrapnel","Tortoise Shield","Golden Axe"]

starterDisplay :: [String]
starterDisplay = ["Bottled Lightning","Butcher's Cleaver","Bombushka","Golden Popcorn","Guidance","Phantasmal Axe","Floating Skull","Salamander Tail","Fork"]

newsaveItems :: [Int]
newsaveItems = [9,9,9,9,9,9,9,9,9,9,9,3,9,9,9,9,9,9,9,9,9,9,9,9,3,3,9,9,9,9,9,9,9,9,3,9,3,9,3,9,9,9,3,9,9,1,3,5,5,3,3,9,9,9,9,9,9,9,3]

--starterItems = [9,9,9,9,9,9,9,9,1]

starterToNewsave y = case y of
     0 -> 5
     1 -> 31
     2 -> 0
     3 -> 22
     4 -> 7
     5 -> 44
     6 -> 25
     7 -> 6
     8 -> 50
     _ -> -1

-- |
-- Backtracking for new save
-- 
-- Note: if user input is -1, due to the laziness of haskell it won't calculate the relics, it'll just skip it
--
-- e.g | newsaveBacktracking [5, -1, -1, -1, -1, 6, 7, -1, -1, -1, -1, -1, -1, 8, -1, -1, -1, -1, -1, -1] 2 -> 
--
-- returns: [seed]
--newsaveBacktracking :: [Int] -> Int -> [Int]
newsaveBacktracking list n = do
    print list
    starterItems' <- starterWeight
    starterItems <- starterItems'
    return $ take n [seed |
        -- TODO: change -1 to a maybe
        let sum0     =  sum starterItems,

        --Seed
        seed <- [10000..99999999],

        --Tutorial
        let x1       =  getIndex (seed + 1) starterItems sum0, --find relic in starter relics
        [x1]         == take 1 list,

        --Mine 2
        let index1   = list !! 1,
        let items1   = replaceAt (starterToNewsave x1) 0 newsaveItems, --replace last found relic to not show up again
        let sum1     = sum items1, --get the sum of items
        let x2       = getIndex (seed + 2) items1 sum1, --find relic specific relic
        x2 == index1 || index1 == (-1), --check if user doesn't want relic or relic is what user wants

        --Mine 3
        let index2   = list !! 2,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 -  sum' index1 x2 items1,
        let x1       = getIndex (seed + 3) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Mine 4
        let index1   = list !! 3,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 4) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Dungeon 1
        let index2   = list !! 4,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 6) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Dungeon 2
        let index1   = list !! 5,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 7) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Dungeon 3
        let index2   = list !! 6,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 8) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Dungeon 4
        let index1   = list !! 7,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 9) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Halls 1
        let index2   = list !! 8,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 11) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Halls 2
        let index1   = list !! 9,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 12) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Halls 3
        let index2   = list !! 10,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 13) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Halls 4
        let index1   = list !! 11,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 14) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Caverns 1
        let index2   = list !! 12,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 16) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Caverns 2
        let index1   = list !! 13,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 17) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Caverns 3
        let index2   = list !! 14,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 18) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Caverns 4
        let index1   = list !! 15,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 19) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Core 1
        let index2   = list !! 16,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 21) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Core 2
        let index1   = list !! 17,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 22) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Core 3
        let index2   = list !! 18,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 23) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Core 4
        let index1   = list !! 19,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 24) items1 sum1,
        x2 == index1 || index1 == (-1)


        ]

newsaveMain = do
    clearScreen
    putStr "\n\n"
    putStr "You are in newsave, if you'd like to quit press CTRL + C,"
    putStr "\nIf you'd like to skip a relic press enter. (after mine 1)"
    putStr "\n\n"
    putStr "\n\n\n\n\n\n\n\n\n\n\n"
    putStr "Pick one of the following: "
    putStr "\n\n"
    print starterDisplay
    putStr "\n"
    print "Mine 1: "
    x1 <- getLine :: IO String
    x1 <- findIndexWithError x1 "Mine 1: " starterDisplay

    x2 <- nextRoom "Mine 2" newsaveDisplay
    x3 <- nextRoom "Mine 3" newsaveDisplay
    x4 <- nextRoom "Mine 4" newsaveDisplay
    x5 <- nextRoom "Dungeon 1" newsaveDisplay
    x6 <- nextRoom "Dungeon 2" newsaveDisplay
    x7 <- nextRoom "Dungeon 3" newsaveDisplay
    x8 <- nextRoom "Dungeon 4" newsaveDisplay
    x9 <- nextRoom "Halls 1" newsaveDisplay
    x10 <- nextRoom "Halls 2" newsaveDisplay
    x11 <- nextRoom "Halls 3" newsaveDisplay
    x12 <- nextRoom "Halls 4" newsaveDisplay
    x13 <- nextRoom "Caverns 1" newsaveDisplay
    x14 <- nextRoom "Caverns 2" newsaveDisplay
    x15 <- nextRoom "Caverns 3" newsaveDisplay
    x16 <- nextRoom "Caverns 4" newsaveDisplay
    x17 <- nextRoom "Core 1" newsaveDisplay
    x18 <- nextRoom "Core 2" newsaveDisplay
    x19 <- nextRoom "Core 3" newsaveDisplay
    x20 <- nextRoom "Core 4" newsaveDisplay

    print "Amount of output: "
    n <- amount "Amount of output: "

    print "This may take a few minutes"
    x <- newsaveBacktracking [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20] n
    appendFile "/Output.txt" $ show x
    timeIt $ print x
    print "Finished"