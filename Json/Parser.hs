{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Json.Parser where

import Basic

import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema
import qualified Data.Text as T
import Data.Typeable
import Data.Aeson.Schema.Key
import Data.Maybe
import Control.Monad
import Control.Lens
import qualified Data.Sequence as S
import Data.List

-- First, define the schema of the JSON data
type MySchema = [schema|
  {
    relics: List {
      guid: Text,
      name: Text,
      display: Text,
      rarity: Text,
      crafting: Int,
      cost: Int,
      extra: String,
      tables: {
        relic: {
            index: Int,
            weight: Int,
            },
      },
    },
  }
|]


type Relics = [schema|
  {
    relics: List {
      guid: Text,
      name: Text,
      display: Text,
      rarity: Text,
      crafting: Int,
      cost: Int,
      extra: String,
      tables: {
        relic: Maybe {
            index: Int,
            weight: Int,
            },
        relicStarter: Maybe {
            index: Int,
            weight: Int,
            },
      },
    },
  }
|]


{- index = do
    obj <- either fail return =<<
        eitherDecodeFileStrict "JsonFiles/Relics.json" :: IO (Object MySchema)

    let index  = [get| obj.relics[].tables.relic.index |]

    return index -}



aids' x = do
  aidz <- x
  return $ [x | Just x <- aidz]

baseJson = do
  obj <- either fail return =<<
    eitherDecodeFileStrict "JsonFiles/Main.json" :: IO (Object Relics)

  return [get| obj.relics[] |]

starter = do
  base <- baseJson
  let starter = map (\ x -> [get| x.tables.relicStarter |]) base
  return $ [x | Just x <- starter]

starterWeight = masterindex starterIndex . map (\ x -> [get| x.weight |]) <$> starter
starterIndex  = map (\ x -> [get| x.index |]) <$> starter

starterDisplay' = do
  baseJson' <- baseJson
  let display = map (\ x -> [get| x.display |]) baseJson'
  let condition = map (\ x -> isJust [get| x.tables.relicStarter |]) baseJson'

  masterindex starterIndex $ remove T.empty $ zipWith isTrue condition display


isTrue x y
  | x         = y
  | otherwise = T.empty

  {- 
maxDisplay' = do
  baseJson' <- baseJson
  let display = map (\ x -> [get| x.display |]) baseJson'
  let condition = map (\ x -> isJust [get| x.tables.relic |]) baseJson'

  masterindex maxIndex $ remove T.empty $ zipWith isTrue condition display

lul x = do
  aidz <- x
  return $ [isJust x | x <- aidz]
  --print $ catMaybes $ aidz

starterIndex = do
  aidzz <- aids' aids
  return $ map (\x -> [get| x.index |]) aidzz

maxIndex = do
  aidzz <- aids' aids
  return $ map (\x -> [get| x.index |]) aidzz

weight = do
  aidzz <- aids' aids
  return $ map (\x -> [get| x.weight |]) aidzz
-}
masterindex x y = do
  x' <- x
  return $ owo111 x' y y


{- 
weight = do
  obj <- either fail return =<<
    eitherDecodeFileStrict "JsonFiles/Relics.json" :: IO (Object MySchema)

  let weight = [get| obj.relics[].tables.relic.weight |]
  return weight
 -}

{- masterindex mIndex = do
  obj <- either fail return =<<
    eitherDecodeFileStrict "JsonFiles/Relics.json" :: IO (Object MySchema)

  let weight = [get| obj.relics[].display |]
  return $ weight !! mIndex -}