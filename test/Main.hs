-- |

module Main where

import           CLITest

import           ComputationTest

import           ParserTest

import           Test.Tasty
import           Test.Tasty.HUnit

main = defaultMain . testCase "Parser.Entry tests:" $ do
  parseTimeTest
  -- dayHeaderTest
  -- productivityHeaderTest
  -- sleepHeaderTest
  -- drinkHeaderTest
  -- meditationHeaderTest
  -- moodHeaderTest
  -- ratingHeaderTest
  -- cigaretteHeaderTest
