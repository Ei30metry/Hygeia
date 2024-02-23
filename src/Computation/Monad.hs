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


class Summarizable a where
  summary :: a -> SummaryType a


instance Summarizable Day where
  summary = (: [])


instance Summarizable [Mood] where
  summary = condenseMoods


instance Summarizable Rating where
  summary = id


instance Summarizable Productivity where
  summary = id


instance Summarizable Sleep where
  summary = id


instance Summarizable [Cigarette] where
  summary = condenseCigarettes


instance Summarizable [Drink] where
  summary = condenseDrinks


instance Summarizable [Day] where
  summary = id


instance Summarizable [Rating] where
  summary xss = (xss, toEnum . (`div` length xss) . sum $ fmap fromEnum xss)


instance Summarizable [Productivity] where
  summary = label fold


instance Summarizable [[Mood]] where
  summary = label (concatMap condenseMoods)


instance Summarizable [Sleep] where
  summary = label (uncurry SP . bimap averageDiffTime averageDiffTime . unzip . map (\(SP x y) -> (x,y)))
    where
      averageDiffTime times
        = secondsToDiffTime .  (`div` (toInteger $ length times)) . sum $ map diffTimeToSeconds times


instance Summarizable [[Drink]] where
  summary = label (concatMap condenseDrinks)


instance Summarizable [[Cigarette]] where
  summary = label (concatMap condenseCigarettes)


instance Summarizable [Meditation] where
  summary = id


instance Summarizable [[Meditation]] where
  summary = label fold


instance Summarizable (Entry Parsed) where
  summary (Entry d m s p me dr c r) =
    Entry (summary d) (summary m) (summary s)
          (summary p) (summary me) (summary dr)
          (summary c) (summary r)


instance Summarizable [Entry Summaraized] where
  summary = fold
