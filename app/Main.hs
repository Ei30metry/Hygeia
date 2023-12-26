module Main where

import           Parser.Entry ( parseDay )

import           Text.Parsec

main :: IO ()
main = case parse parseDay "" "2023-13-19" of
          Right x -> print x
          Left y  -> print y
