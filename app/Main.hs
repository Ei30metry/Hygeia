{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CLI

import           Computation.Monad
import           Computation.Utils

import           Config

import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Text              ( Text )

import           System.Directory

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
Version 3: TUI! that would be very clean!
-}

main = do
  action <- cli
  createDirectoryIfMissing True (_entryDirectory defaultConfig)
  putStrLn "built Hygeia!"



parseEntryTest :: IO ()
parseEntryTest = do
  sample <- B.readFile "/Users/artin/Programming/projects/Hygeia/test/sample/pro.txt"
  case runParser parseProductivity sample of
    Right x -> print x
    Left y  -> putStrLn y
