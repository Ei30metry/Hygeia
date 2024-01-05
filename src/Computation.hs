module Computation (module Computation.Util
                   ,module Computation.Types
                   ,module Computation.Monad) where

import           Computation.Monad
import           Computation.Types
import           Computation.Util

import           Data.Vector                ( Vector )

import           GHC.Generics


-- a Type representing one's mood with it's singleton definitions
