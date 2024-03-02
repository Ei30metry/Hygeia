-- |

module Main where

import           CLITest

import           ComputationTest

import           ParserTest

import           Test.Tasty
import           Test.Tasty.Hspec

main = do
  spec <- testSpec "Parser.Entry tests:" spec_Parser_Entry
  defaultMain (testGroup "tests" [spec])
