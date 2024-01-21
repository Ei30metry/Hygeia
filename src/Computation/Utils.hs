-- | Simple utility functions

module Computation.Utils where

import           Config                ( DaemonConf (..) )

import           Control.Applicative
import           Control.Lens

import           Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as B
import qualified Data.ListLike         as L
import           Data.Maybe
import           Data.Time             ( DiffTime )
import           Data.Vector           ( Vector )



label f x = (x, f x)
