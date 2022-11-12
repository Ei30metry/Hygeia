module Main where

import           Parser.Input (parseEntry)
import           Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = do
  file1 <- readFile "/Users/artin/Programming/projects/Hygeia/test/sample.txt"
  let result1 = parse parseEntry "error" file1
  print result1
