{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CLI

import           Computation.Utils

import           Config

import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Text              ( Text )
import           Data.YAML

import           Parser.Entry           ( parseDay, parseEntry,
                                          parseMeditations, parseProductivity )
import           Parser.Monad           ( runParser )

import           Prettyprinter
import           Prettyprinter.Util

import           Text.Parsec            ( parse, runParserT )
import           Text.Parsec.ByteString

{-
Version 1: Only print summary.
Version 2: Print summary, if verbose flag is on, print exact entries too.
-}

main = mainEntry

mainEntry :: IO ()
mainEntry = do
  sample <- B.readFile "/Users/artin/Programming/projects/Hygeia/test/sample.txt"
  case runParser parseEntry sample of
    Right x -> putDocW 120 . pretty . summary $ x
    Left y  -> putStrLn y


mainConfig :: IO ()
mainConfig = do
  sampleConfig <- BL.readFile "/Users/artin/Programming/projects/Hygeia/test/sampleconfig"
  let val = decode1 sampleConfig :: Either (Pos,String) Config
  print val
