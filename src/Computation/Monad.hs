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

import           Data.Bifunctor
import           Data.ByteString.Lazy.Char8 ( ByteString )
import           Data.Coerce
import           Data.Foldable
import           Data.Kind
import           Data.List                  ( groupBy, sort )
import           Data.Time                  ( Day, DiffTime, secondsToDiffTime )

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


saveEntry :: Entry Summaraized -> IO ()
saveEntry = undefined


combineEntries :: Entry Summaraized -> Entry Summaraized ->  Maybe (Entry Summaraized)
combineEntries = undefined


withComp = withReaderT


mapComp = mapReaderT
