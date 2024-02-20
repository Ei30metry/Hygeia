{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CLI

import           Config

import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Text              ( Text )
import           Data.YAML

import           Parser.Entry           ( parseDay, parseEntry,
                                          parseMeditations, parseProductivity )
import           Parser.Monad           ( runParser )

import           Text.Parsec            ( parse, runParserT )
import           Text.Parsec.ByteString

{-
Version 1: Only print summary.
Version 2: Print summary, if verbose flag is on, print exact entries too.
-}

main = do
  action <- cli
  print action

mainEntry :: IO ()
mainEntry = do
  sample <- B.readFile "/Users/artin/Programming/projects/Hygeia/test/sample2.txt"
  case runParser parseEntry sample of
    Right _ -> putStrLn "Everything is cool"
    Left y  -> putStrLn y


mainConfig :: IO ()
mainConfig = do
  sampleConfig <- BL.readFile "/Users/artin/Programming/projects/Hygeia/test/sampleconfig"
  let val = decode1 sampleConfig :: Either (Pos,String) Config
  print val
