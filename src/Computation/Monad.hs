-- | The monad used by Hygeia to compute entry summaries

module Computation.Monad where

import           Computation.Types

import qualified Config                     as C

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer

import           Daemon

import           Data.ByteString.Lazy.Char8 ( ByteString )
import           Data.Functor
import           Data.Time                  ( Day, UTCTime, getCurrentTime,
                                              utctDay )

import           System.Info

import           Template


data CompError


type Days = Int


data Action = Summary [C.EntryField] C.Interval
            | Lookup [C.EntryField] C.Interval
            | Config C.ConfCommand
            | Generate C.Interval
            | Daemon C.DaemonCommand
            deriving (Show, Eq)


type Comp e r = ReaderT e (Except CompError) r


data Env = Env { envConf       :: C.Config
               , action        :: Action
               , firstEntryDay :: Maybe Day }
         deriving (Show, Eq)

-- NOTE This should query the sqlite database
buildInitialEnv :: Action -> IO Env
buildInitialEnv ac = Env C.defaultConfig ac <$> getFirstDay
  where getFirstDay = pure undefined


saveEntry :: Entry Summaraized -> IO ()
saveEntry = undefined


getEntry :: a -> Maybe (Entry Summaraized)
getEntry = undefined


withComp = withReaderT


mapComp = mapReaderT


runAction :: Env -> IO ()
runAction (Env conf action firstDay) = do
  today <- utctDay <$> getCurrentTime
  case action of
      Generate interval -> undefined -- TODO


runComputation :: Comp Env ()
runComputation = undefined
