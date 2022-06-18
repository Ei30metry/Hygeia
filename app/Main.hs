module Main where

import           Computation
import           Parser
import           System.Environment
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Token

main :: IO ()
main = do
  file1 <- readFile "/Users/artin/Documents/coding/projects/Hygeia/test/sample.txt"
--  file2 <- readFile "/Users/artin/Documents/coding/projects/Hygeia/test/sample2.txt"
  let result1 = parse parseEntry "error" file1
 -- let result2 = parse parseEntry "error" file2
  print result1
  --print result2
  return ()
