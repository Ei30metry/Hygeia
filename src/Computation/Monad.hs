-- | The monad used by Hygeia to compute a summary for the entries

module Computation.Monad where

import           Computation.Types
import           Computation.Utils

import qualified Config                     as C

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer

import           Daemon

import           Data.ByteString.Lazy.Char8 ( ByteString )
import           Data.Coerce
import           Data.Foldable
import           Data.Kind
import           Data.Time                  ( Day )

import           System.Info


data CompError

type Days = Int

data Action = Summary [C.EntryField] C.Interval
            | Config C.ConfCommand
            | Generete C.Interval
            | Daemon C.DaemonCommand
            deriving Show


defaultConfig :: C.Config
defaultConfig = C.Config userInfo' daemonConf' templateConf' optHeader' "/Users/artin/Documents/Hygeia/"
  where
    osInfo'       = C.OsInfo os (serviceManager os)
    daemonConf'   = C.DaemonConf True osInfo'
    optHeader'    = C.OptH True True True
    templateConf' = C.TempConf True
    userInfo'     = C.Info "Unknown"

-- type Comp a = ReaderT Env (Except CompError) a
type Comp a = ReaderT C.Config (Except CompError) a

runComp :: Comp a -> C.Config -> Either CompError a
runComp comp = runExcept . runReaderT comp

-- saveEntry :: Entry -> IO ()
saveEntry = undefined

combineEntries = undefined

initialEntry = undefined


withComp = withReaderT

mapComp = mapReaderT




class Summarizable a where
  summary :: a -> SummaryType (UnList a)

instance {-# OVERLAPPABLE #-} ((SummaryType (UnList a)) ~ a) => Summarizable a where
  summary = id

instance Summarizable [Rating] where
  summary xss = toEnum . (`div` length xss) . sum $ fmap fromEnum xss

instance Summarizable [Productivity] where
  summary = label fold

instance Summarizable [Moods] where
  summary = undefined

instance Summarizable [Sleep] where
  summary = label averageSleepTime
    where
      averageSleepTime :: [Sleep] -> Sleep
      averageSleepTime = undefined

instance Summarizable [Drinks] where
  summary = undefined

instance Summarizable [Cigarette] where
  summary = undefined

instance Summarizable [Meditations] where
  summary = label fold

-- TODO Use the trees that grow approach in Entry
instance Summarizable (Entry Summaraizer) where
  summary (Entry d m s p me dr c r) = Entry d (summary m) (summary s) (summary p) (summary me) (summary dr) (summary c) (summary r)
