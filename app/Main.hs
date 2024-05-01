{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CLI

import           Computation.Monad
import           Computation.Utils

import           Config

import           Control.Monad

import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Maybe
import           Data.Text              ( Text )
import qualified Data.Text              as T

import           Database.SQLite.Simple

import           Parser.Entry           ( parseDay, parseEntry,
                                          parseMeditations, parseProductivity )
import           Parser.Monad           ( runParser )

import           Prettyprinter
import           Prettyprinter.Util

import           System.Directory
import           System.Posix.Files
import           System.Process

import           Text.Parsec            ( parse, runParserT )
import           Text.Parsec.ByteString

{-
Version 1: Only print summary.
Version 2: Print summary, if verbose flag is on, print exact entries too.
Version 3: TUI
-}

doesFirstEntryExist :: Connection -> IO ()
doesFirstEntryExist = undefined


main = do
  action <- cli
  env <- buildInitialEnv action
  let dir = _entryDirectory (envConf env)
  let dbpath = dir ++ ".hygeia.db"
  createDirectoryIfMissing True (_entryDirectory (envConf env))
  doesFileExist dbpath >>= \val -> unless val (writeFile dbpath "")
  putStrLn dbpath
  handle <- open dbpath
  close handle
  runAction env
