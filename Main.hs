{-# LANGUAGE OverloadedStrings #-}

import Json.Parser
import Basic

import System.Console.ANSI

import Modes.Newsave.Newsave

main = do
    clearScreen
    putStr "\n\n"
    putStr "Hello!"
    putStr "\n\nThis program finds specific seeds that you can use in undermine."
    putStr "\nKeep in mind that there are not "
    setSGR [SetColor Foreground Vivid Red]
    putStr "nearly enough "
    setSGR [Reset]
    putStr "seeds for combinations exceeding 5 relics"
    putStr "\n\n"
    putStr "Type either \"newsave\" or \"100\" for the right save file"
    putStr "\n\n\n\n\n\n\n\n\n\n\n\n"
    newsave
    
newsave = newsaveMain