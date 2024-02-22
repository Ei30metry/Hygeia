-- | Simple utility functions

module Computation.Utils where

import           Config                ( DaemonConf (..) )

import           Control.Applicative
import           Control.Lens

import           Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Data.Time             ( Day, DiffTime, diffTimeToPicoseconds )


label f x = (x, f x)

diffTimeToSeconds = (* 10^12) . diffTimeToPicoseconds
