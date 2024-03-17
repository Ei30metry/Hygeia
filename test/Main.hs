-- |

module Main where

import           CLITest

import           ComputationTest

import           ConfigTest

import           ParserTest

import           Test.Tasty
import           Test.Tasty.Hspec

main = do
  parserSpec <- testSpec "Parser.Entry tests:" spec_Parser_Entry
  -- cliSpec <- testSpec "CLI tests:" spec_CLI
  configSpec <- testSpec "Config tests:" spec_Config
  defaultMain (testGroup "tests" [parserSpec,configSpec])
