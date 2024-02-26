-- | Unit tests for the Config module


module ConfigTest where

import           Config

import qualified Data.ByteString.Lazy as BL
import           Data.YAML

import           Test.Tasty.HUnit

-- TODO Turn this into an assertion
userWrittenConfigTest :: Assertion
userWrittenConfigTest = do
  sampleConfig <- BL.readFile "/Users/artin/Programming/projects/Hygeia/test/sampleconfig"
  let val = decode1 sampleConfig :: Either (Pos,String) Config
  print val
