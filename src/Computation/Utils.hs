-- | Simple utility functions

module Computation.Utils where

import           Config                ( DaemonConf (..) )

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Extra

import           Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as B
import           Data.Maybe

-- | Safe head
head' []     = Nothing
head' (x:xs) = Just x

