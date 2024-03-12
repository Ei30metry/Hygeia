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


defaultConfig :: C.Config
defaultConfig = C.Config userInfo' daemonConf' templateConf' optHeader' "/Users/artin/Documents/Hygeia/"
  where
    osInfo'       = C.OsInfo os (serviceManager os)
    daemonConf'   = C.DaemonConf True osInfo'
    optHeader'    = C.OptH True True True
    templateConf' = C.TempConf True
    userInfo'     = C.Info "Unknown"


type Comp e r = ReaderT e (Except CompError) r


data Env = Env { envConf :: C.Config
               , action  :: Action } deriving (Show, Eq)

-- TODO This has to be runComp
-- runAction :: MonadIO m => Comp Env () -> m ()
-- runAction a c = writeTemplates a c
-- runAction _ c = undefined


writeTemplates :: C.Interval -> C.Config -> IO ()
writeTemplates ac c =
  case ac of
    C.DefInterval C.Today -> do
      day <- utctDay <$> getCurrentTime
      runReaderT (writeTemplate day) c
      putStrLn $ "Wrote entry file for " ++ show day
    _ -> putStrLn "Blah"

-- TODO This needs to be fixed
-- runComp :: Comp a -> C.Config -> Either CompError a
-- runComp comp = runExcept . runReaderT comp


saveEntry :: Entry Summaraized -> IO ()
saveEntry = undefined


getEntry :: a -> Maybe (Entry Summaraized)
getEntry = undefined


withComp = withReaderT


mapComp = mapReaderT
