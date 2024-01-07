-- |

module Computation.Monad where

import           Config

import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer

import           Data.ByteString.Lazy.Char8 ( ByteString )
import           Data.Vector                ( Vector )

data CompError

type Comp a = ReaderT Config (Except CompError) a

runComp :: Comp a -> Config -> Either CompError a
runComp comp = runExcept . runReaderT comp

-- saveEntry :: Entry -> IO ()
saveEntry = undefined

combineEntries = undefined

initialEntry = undefined

-- makeSenseOfEntries :: Vector Entry -> Entry
-- makeSenseOfEntries = foldr combineEntries initialEntry

withComp = withReaderT

mapComp = mapReaderT
