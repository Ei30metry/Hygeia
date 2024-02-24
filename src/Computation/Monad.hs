-- | The monad used by Hygeia to compute entry summaries

module Computation.Monad where

import           Computation.Types

import qualified Config                     as C

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer

import           Daemon

import           Data.ByteString.Lazy.Char8 ( ByteString )

import           System.Info


data CompError

type Days = Int

data Action = Summary [C.EntryField] C.Interval
            | Lookup [C.EntryField] C.Interval
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


type Comp a = ReaderT C.Config (Except CompError) a


runComp :: Comp a -> C.Config -> Either CompError a
runComp comp = runExcept . runReaderT comp


saveEntry :: Entry Summaraized -> IO ()
saveEntry = undefined


getEntry :: a -> Maybe (Entry Summaraized)
getEntry = undefined


withComp = withReaderT


mapComp = mapReaderT
