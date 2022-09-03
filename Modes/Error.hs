module Modes.Error where

import Data.List ( elemIndex )
import System.Console.ANSI
import Text.Read (readMaybe)
import Data.Char (toLower)
import Control.Applicative (liftA2)

import Basic

isInList name list = elemIndex (filterToDefault $ map toLower name) (map filterToDefault $ listToLower list)

filterToDefault = filter ((/= '\'') <&&> (/= ' '))
    where
        (<&&>) = liftA2 (&&)

findIndex' :: String -> String -> [String] -> IO Int
findIndex' name msg list = do
    if null name
        then
            return (-1)
        else do
            case isInList name list of
                Just x -> return x
                Nothing -> do
                    print "ERROR: Wrong input"
                    print msg
                    x <- getLine :: IO String
                    findIndex' x msg list

findIndexWithError :: String -> String -> [String] -> IO Int
findIndexWithError name msg list = do
    case isInList name list of
        Just x -> return x
        Nothing -> do
            print "ERROR: Wrong input"
            print msg
            x <- getLine :: IO String
            findIndexWithError x msg list

nextRoom :: String -> [String] -> IO Int
nextRoom room list = do
    clearScreen

    putStr "Pick one of the following: "
    putStr "\n\n"
    print list
    putStr "\n"

    print $ room ++ ": "
    x <- getLine :: IO String

    findIndex' x (room ++ ": ") list

amount msg = do
    x <- getLine
    let y = readMaybe x :: Maybe Int
    case y of
        Just x -> return x
        Nothing -> do
            print "ERROR: Wrong input \n\n"
            print msg
            amount msg