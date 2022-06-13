module Main where

import           Computation
import           Parser
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Token

main :: IO ()
main = do
  file <- readFile "sample.txt"
  let result = parse parseEntry "error" file
  print result
  return ()
