-- | Simple utility functions

module Computation.Utils where

import           Config                ( DaemonConf (..) )

import           Control.Applicative
import           Control.Lens

import           Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Data.Time             ( DiffTime )


label f x = (x, f x)


type family UnList a where
  UnList [a] = a
  UnList a   = a
