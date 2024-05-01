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

import           Database.SQLite.Simple

import           System.Directory
import           System.Info
import           System.Posix.Files
import           System.Process

import           Template


data CompError


type Days = Int


data Action
  = Summary [C.EntryField] C.Interval
  | Lookup [C.EntryField] C.Interval
  | Config C.ConfCommand
  | Generate C.Interval
  deriving (Show, Eq)


type Comp e r = ReaderT e (Except CompError) r


data Env =
  Env
    { envConf       :: C.Config
    , action        :: Action
    , firstEntryDay :: Maybe Day
    }
  deriving (Show, Eq)

-- NOTE This should query the sqlite database
buildInitialEnv :: Action -> IO Env
buildInitialEnv ac = Env C.defaultConfig ac <$> getFirstEntryDate


getFirstEntryDate :: IO (Maybe Day)
getFirstEntryDate = undefined

-- TODO
initiateDB :: FilePath -> IO Connection
initiateDB dbpath = do
  doesFileExist dbpath >>= \p -> unless p (writeFile dbpath "")
  conn <- open dbpath
  --execute_ conn "CREATE TABLE IF NOT EXISTS summary (id )"
  return conn


saveEntry :: Entry Summaraized -> IO ()
saveEntry = undefined


getEntry :: Day -> Maybe (Entry Summaraized)
getEntry = undefined


withComp = withReaderT


mapComp = mapReaderT

-- queries the database
lookupEntries = undefined


runAction :: Env -> IO ()
runAction (Env conf action firstDay) = do
  today <- utctDay <$> getCurrentTime
  let firstEntryDate = read "2023-06-02"
  case action of
    Generate interval ->
      runReaderT (writeTemplates interval firstEntryDate) conf
    Summary entryFields interval -> putStrLn ("Summary of " <> show interval)
    Lookup entryFields interval ->
      runReaderT (lookupEntries interval entryFields firstEntryDate) conf
    Config command -> putStrLn ("Command: " <> show command)


runComputation :: Comp Env ()
runComputation = undefined
