{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8  as B

import           Parser.Entry           ( parseDay, parseEntry)
import           Parser.Monad           ( runParser )

import           Text.Parsec            ( parse, runParserT )
import           Text.Parsec.ByteString


main :: IO ()
main = do
  sample <- B.readFile "/Users/artin/Programming/projects/Hygeia/test/sample2.txt"
  case runParser parseEntry sample of
    Right x -> print x
    Left y  -> putStrLn y
