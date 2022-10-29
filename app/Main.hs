module Main where

import           Computation
import           Parser.Input
import           System.Environment
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Token

main :: IO ()
main = do
  file1 <- readFile "/Users/artin/Programming/projects/Hygeia/test/sample.txt"
  let result1 = parse parseEntry "error" file1
  print result1
