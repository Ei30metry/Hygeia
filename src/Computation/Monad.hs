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
import           Data.List                  ( groupBy )
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


saveEntry :: Entry Summaraizer -> IO ()
saveEntry = undefined


combineEntries :: Entry Summaraizer -> Entry Summaraizer -> Maybe (Entry Summaraizer)
combineEntries = undefined


withComp = withReaderT


mapComp = mapReaderT


class Summarizable a where
  summary :: a -> SummaryType (UnList a)

instance {-# OVERLAPPABLE #-} ((SummaryType (UnList a)) ~ a) => Summarizable a where
  summary = id


instance Summarizable [Day] where
  summary = id


instance Summarizable [Rating] where
  summary xss = (xss, toEnum . (`div` length xss) . sum $ fmap fromEnum xss)


instance Summarizable [Productivity] where
  summary = label fold


instance Summarizable [Moods] where
  summary = label (condenseMoods . Moods . concatMap unMoods)


instance Summarizable [Sleep] where
  summary = label (uncurry SP . bimap averageDiffTime averageDiffTime . unzip . map (\(SP x y) -> (x,y)))
    where
      averageDiffTime times
        = secondsToDiffTime .  (`div` (toInteger $ length times)) . sum $ map diffTimeToSeconds times


instance Summarizable [Drinks] where
  summary = label (coerce . concatMap (flattenDrinks . coerce))
    where
      flattenDrinks = map (foldr unsafeAddShots (Alcohol "" 0)) . groupBy sameDrink


instance Summarizable [Cigarettes] where
  summary = label (coerce . concatMap (flattenCigarettes . coerce))
    where
      flattenCigarettes = map (foldr unsafeAddSmokes (Cigarette "" 0 0 0)) . groupBy sameCigarette


instance Summarizable [Meditations] where
  summary = label fold

-- TODO Use the trees that grow approach in Entry
instance Summarizable (Entry Parser) where
  summary (Entry d m s p me dr c r) = Entry undefined undefined undefined undefined undefined undefined undefined undefined -- Entry (summary d) (summary m) (summary s) (summary p) (summary me) (summary dr) (summary c) (summary r)


instance Summarizable [Entry Summaraizer] where
  summary = undefined
