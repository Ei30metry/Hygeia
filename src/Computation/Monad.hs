-- |

module Computation.Monad where

import           Config

import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer

import           Data.ByteString.Lazy.Char8 ( ByteString )

data CompError


type Comp a = ReaderT Config (Except CompError) a

runComp :: Comp a -> Config -> Either CompError a
runComp comp = runExcept . runReaderT comp

withComp = withReaderT

mapComp = mapReaderT
